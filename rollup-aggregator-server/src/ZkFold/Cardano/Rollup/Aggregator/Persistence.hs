module ZkFold.Cardano.Rollup.Aggregator.Persistence (
  PersistedState (..),
  initDb,
  enqueueTxDb,
  dequeueTxsDb,
  recordBatchDb,
  revertTxsDb,
  getTxByHashDb,
  getPendingTxsDb,
  getTxsByAddressDb,
  getBatchesDb,
  getBatchByIdDb,
  getPendingBridgeOutsDb,
  saveState,
  loadState,
) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString.Lazy (toStrict)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Database.SQLite.Simple
import Deriving.Aeson
import GHC.Natural (Natural)
import GeniusYield.Types (GYAddress, LowerFirst)
import ZkFold.Cardano.Rollup.Aggregator.Types
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Symbolic.Ledger.Types

-- | State persisted to the SQLite database across restarts.
data PersistedState = PersistedState
  { psLedgerState ∷ !(State Bi Bo Ud A I)
  , psUtxoPreimage ∷ !(Leaves Ud (UTxO A I))
  }
  deriving stock Generic
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "ps", LowerFirst]] PersistedState

withConn ∷ FilePath → (Connection → IO a) → IO a
withConn dbPath = bracket (open dbPath) close

-- | Initialise the SQLite database: enable WAL mode and create tables.
initDb ∷ FilePath → IO ()
initDb dbPath = withConn dbPath $ \conn → do
  execute_ conn "PRAGMA journal_mode = WAL"
  -- Keep old table for zero-downtime schema migration on existing DBs.
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS pending_txs \
    \(id INTEGER PRIMARY KEY AUTOINCREMENT, payload TEXT NOT NULL)"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS ledger_state \
    \(id INTEGER PRIMARY KEY CHECK (id = 1), \
    \ledger_state TEXT NOT NULL, \
    \utxo_preimage TEXT NOT NULL)"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS txs \
    \(id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \tx_hash TEXT NOT NULL UNIQUE, \
    \payload TEXT NOT NULL, \
    \status TEXT NOT NULL DEFAULT 'pending', \
    \batch_id INTEGER, \
    \submitted_at TEXT NOT NULL)"
  execute_
    conn
    "CREATE INDEX IF NOT EXISTS idx_txs_hash ON txs(tx_hash)"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS batches \
    \(id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \l1_tx_id TEXT NOT NULL, \
    \created_at TEXT NOT NULL, \
    \tx_count INTEGER NOT NULL)"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS tx_addresses \
    \(tx_id INTEGER NOT NULL REFERENCES txs(id), \
    \l2_address TEXT NOT NULL)"
  execute_
    conn
    "CREATE INDEX IF NOT EXISTS idx_tx_addresses ON tx_addresses(l2_address)"

-- | Enqueue a single transaction. Computes SHA256 of the JSON payload as the
-- transaction hash, stores L2 addresses for indexed lookup, and returns the hash.
enqueueTxDb ∷ FilePath → QueuedTx → [Text] → IO Text
enqueueTxDb dbPath qtx addrs = withConn dbPath $ \conn →
  withTransaction conn $ do
    now ← getCurrentTime
    let payloadBytes = toStrict (encode qtx)
        txHash = decodeUtf8 . toStrict . encode $ txId (qtTransaction qtx)
    execute
      conn
      "INSERT INTO txs (tx_hash, payload, status, submitted_at) VALUES (?, ?, 'pending', ?)"
      (txHash, decodeUtf8 payloadBytes, formatTimestamp now)
    rowId ← lastInsertRowId conn
    forM_ addrs $ \addr →
      execute
        conn
        "INSERT INTO tx_addresses (tx_id, l2_address) VALUES (?, ?)"
        (rowId, addr)
    return txHash

-- | Atomically dequeue exactly @n@ transactions.
-- Returns 'Nothing' if fewer than @n@ are available (rows stay 'pending').
-- Otherwise marks them 'processing' and returns (id, QueuedTx) pairs.
dequeueTxsDb ∷ FilePath → Natural → IO (Maybe [(Int64, QueuedTx)])
dequeueTxsDb dbPath n = withConn dbPath $ \conn →
  withExclusiveTransaction conn $ do
    rows ∷ [(Int64, Text)] ←
      query
        conn
        "SELECT id, payload FROM txs WHERE status='pending' ORDER BY id LIMIT ?"
        (Only (fromIntegral n ∷ Int))
    if length rows < fromIntegral n
      then return Nothing
      else do
        let decoded = sequence [eitherDecodeStrict (encodeUtf8 payload) | (_, payload) ← rows]
        case decoded of
          Left _ → return Nothing
          Right qtxs → do
            forM_ rows $ \(rowId, _) →
              execute
                conn
                "UPDATE txs SET status='processing' WHERE id=?"
                (Only rowId)
            return (Just (zip (map fst rows) qtxs))

-- | Record a successfully submitted batch: insert a 'batches' row and mark
-- all included txs as 'batched'.
recordBatchDb ∷ FilePath → [Int64] → Text → IO ()
recordBatchDb dbPath ids l1TxId = withConn dbPath $ \conn →
  withTransaction conn $ do
    now ← getCurrentTime
    execute
      conn
      "INSERT INTO batches (l1_tx_id, created_at, tx_count) VALUES (?, ?, ?)"
      (l1TxId, formatTimestamp now, length ids)
    batchId ← lastInsertRowId conn
    forM_ ids $ \tid →
      execute
        conn
        "UPDATE txs SET status='batched', batch_id=? WHERE id=?"
        (batchId, tid)

-- | Revert 'processing' txs back to 'pending' (called on batch failure).
revertTxsDb ∷ FilePath → [Int64] → IO ()
revertTxsDb dbPath ids = withConn dbPath $ \conn →
  withTransaction conn $
    forM_ ids $ \tid →
      execute
        conn
        "UPDATE txs SET status='pending', batch_id=NULL WHERE id=?"
        (Only tid)

-- | Look up a single transaction by its SHA256 hash.
getTxByHashDb ∷ FilePath → Text → IO (Maybe TxRecord)
getTxByHashDb dbPath txHash = withConn dbPath $ \conn → do
  rows ∷ [(Int64, Text, Text, Text, Maybe Int64, Text)] ←
    query
      conn
      "SELECT id, tx_hash, payload, status, batch_id, submitted_at FROM txs WHERE tx_hash=?"
      (Only txHash)
  case rows of
    [row] → return (parseTxRow row)
    _ → return Nothing

-- | Return all currently pending transactions.
getPendingTxsDb ∷ FilePath → IO [TxRecord]
getPendingTxsDb dbPath = withConn dbPath $ \conn → do
  rows ∷ [(Int64, Text, Text, Text, Maybe Int64, Text)] ←
    query_
      conn
      "SELECT id, tx_hash, payload, status, batch_id, submitted_at FROM txs WHERE status='pending' ORDER BY id"
  return (catMaybes (map parseTxRow rows))

-- | Paginated tx history for an L2 address (JSON-encoded FieldElement text).
getTxsByAddressDb ∷ FilePath → Text → Natural → Natural → IO [TxRecord]
getTxsByAddressDb dbPath l2addr limit offset = withConn dbPath $ \conn → do
  rows ∷ [(Int64, Text, Text, Text, Maybe Int64, Text)] ←
    query
      conn
      "SELECT id, tx_hash, payload, status, batch_id, submitted_at \
      \FROM txs \
      \WHERE id IN (SELECT DISTINCT tx_id FROM tx_addresses WHERE l2_address=?) \
      \ORDER BY id DESC LIMIT ? OFFSET ?"
      (l2addr, fromIntegral limit ∷ Int, fromIntegral offset ∷ Int)
  return (catMaybes (map parseTxRow rows))

-- | Paginated batch list, newest first.
getBatchesDb ∷ FilePath → Natural → Natural → IO [BatchRecord]
getBatchesDb dbPath limit offset = withConn dbPath $ \conn → do
  rows ∷ [(Int64, Text, Text, Int)] ←
    query
      conn
      "SELECT id, l1_tx_id, created_at, tx_count FROM batches ORDER BY id DESC LIMIT ? OFFSET ?"
      (fromIntegral limit ∷ Int, fromIntegral offset ∷ Int)
  return (catMaybes (map parseBatchRow rows))

-- | Look up a batch by id together with all its transactions.
getBatchByIdDb ∷ FilePath → Int64 → IO (Maybe (BatchRecord, [TxRecord]))
getBatchByIdDb dbPath batchId = withConn dbPath $ \conn → do
  batchRows ∷ [(Int64, Text, Text, Int)] ←
    query
      conn
      "SELECT id, l1_tx_id, created_at, tx_count FROM batches WHERE id=?"
      (Only batchId)
  case batchRows of
    [brow] → case parseBatchRow brow of
      Nothing → return Nothing
      Just br → do
        txRows ∷ [(Int64, Text, Text, Text, Maybe Int64, Text)] ←
          query
            conn
            "SELECT id, tx_hash, payload, status, batch_id, submitted_at FROM txs WHERE batch_id=? ORDER BY id"
            (Only batchId)
        let txs = catMaybes (map parseTxRow txRows)
        return (Just (br, txs))
    _ → return Nothing

-- | Bridge-outs (pending + batched) for a given L1 address.
-- Decodes each QueuedTx and filters bridge-out entries matching the address.
getPendingBridgeOutsDb ∷ FilePath → GYAddress → IO [BridgeOutEntry]
getPendingBridgeOutsDb dbPath targetAddr = withConn dbPath $ \conn → do
  rows ∷ [(Int64, Text, Text, Text, Maybe Int64, Text)] ←
    query_
      conn
      "SELECT id, tx_hash, payload, status, batch_id, submitted_at \
      \FROM txs WHERE status IN ('pending', 'batched') ORDER BY id"
  let allTxs = catMaybes (map parseTxRow rows)
  return $ do
    tr ← allTxs
    let qtx = trPayload tr
    (val, addr) ← qtBridgeOuts qtx
    if addr == targetAddr
      then [BridgeOutEntry {boeTxHash = trHash tr, boeValue = val, boeStatus = trStatus tr}]
      else []

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

formatTimestamp ∷ UTCTime → Text
formatTimestamp = Text.pack . iso8601Show

parseTimestamp ∷ Text → Maybe UTCTime
parseTimestamp = iso8601ParseM . Text.unpack

-- Column order: id, tx_hash, payload, status, batch_id, submitted_at
parseTxRow ∷ (Int64, Text, Text, Text, Maybe Int64, Text) → Maybe TxRecord
parseTxRow (rowId, txHash, payload, statusText, batchId, submittedAtText) = do
  qtx ← case eitherDecodeStrict (encodeUtf8 payload) of
    Right x → Just x
    Left _ → Nothing
  st ← txStatusFromText statusText
  ts ← parseTimestamp submittedAtText
  return
    TxRecord
      { trId = rowId
      , trHash = txHash
      , trStatus = st
      , trBatchId = batchId
      , trSubmittedAt = ts
      , trPayload = qtx
      }

parseBatchRow ∷ (Int64, Text, Text, Int) → Maybe BatchRecord
parseBatchRow (rowId, l1TxId, createdAtText, txCount) = do
  ts ← parseTimestamp createdAtText
  return
    BatchRecord
      { brId = rowId
      , brL1TxId = l1TxId
      , brCreatedAt = ts
      , brTxCount = txCount
      }

txStatusFromText ∷ Text → Maybe TxStatus
txStatusFromText "pending" = Just TxPending
txStatusFromText "processing" = Just TxProcessing
txStatusFromText "batched" = Just TxBatched
txStatusFromText _ = Nothing

-- | Persist ledger state and UTxO preimage to the database (single-row upsert).
saveState ∷ FilePath → State Bi Bo Ud A I → Leaves Ud (UTxO A I) → IO ()
saveState dbPath ledgerState utxoPreimage = withConn dbPath $ \conn →
  execute
    conn
    "INSERT OR REPLACE INTO ledger_state (id, ledger_state, utxo_preimage) VALUES (1, ?, ?)"
    (toText ledgerState, toText utxoPreimage)
 where
  toText ∷ ToJSON a ⇒ a → Text
  toText = decodeUtf8 . toStrict . encode

-- | Load persisted state. Returns 'Nothing' if absent or if decoding fails.
loadState ∷ FilePath → IO (Maybe PersistedState)
loadState dbPath = withConn dbPath $ \conn → do
  rows ∷ [(Text, Text)] ←
    query_ conn "SELECT ledger_state, utxo_preimage FROM ledger_state WHERE id = 1"
  case rows of
    [(stText, utxoText)] →
      case
        ( eitherDecodeStrict (encodeUtf8 stText)
        , eitherDecodeStrict (encodeUtf8 utxoText)
        )
        of
          (Right st, Right utxo) → return (Just (PersistedState st utxo))
          _ → return Nothing
    _ → return Nothing
