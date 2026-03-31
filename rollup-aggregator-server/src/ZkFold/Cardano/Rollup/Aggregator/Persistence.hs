module ZkFold.Cardano.Rollup.Aggregator.Persistence (
  PersistedState (..),
  initDb,
  enqueueTxDb,
  dequeueTxsDb,
  dequeueAvailableTxsDb,
  recordBatchDb,
  revertTxsDb,
  revertProcessingTxsDb,
  getPendingTxsWithIdsDb,
  failTxsDb,
  getTxByHashDb,
  getPendingTxsDb,
  getTxsByAddressDb,
  getBatchesDb,
  getBatchByIdDb,
  getPendingBridgeOutsDb,
  saveState,
  loadState,
  savePreimagesDb,
  lookupPreimagesDb,
  lookupPreimagesByRefDb,
  getKnownRefsDb,
  seedPreimageDbFromOldState,
) where

import Control.Exception (bracket)
import Control.Monad (forM, forM_, when)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString.Lazy (toStrict)
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
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
import ZkFold.Cardano.Rollup.Api.Utils (feToInteger)
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Data.Vector (fromVector)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hash (hHash), hash)
import ZkFold.Symbolic.Ledger.Types

-- | State persisted to the SQLite database across restarts.
-- Stores the rollup 'State' and leaf hashes (not full UTxO preimages).
-- Full preimages are stored separately in the @utxo_preimages@ table.
data PersistedState = PersistedState
  { psLedgerState ∷ !(State I)
  , psLeafHashes  ∷ !(Leaves Ud (FieldElement I))
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
  -- Preimage DB: maps leaf hash → full UTxO object.
  -- Written by the Batcher after computing a batch; read by the Batcher
  -- when constructing the preimage vector for the next batch.
  -- Append-only (a UTxO's hash never changes), so no rollback needed.
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS utxo_preimages \
    \(leaf_hash TEXT PRIMARY KEY, \
    \output_ref TEXT NOT NULL, \
    \utxo_data TEXT NOT NULL)"
  execute_
    conn
    "CREATE INDEX IF NOT EXISTS idx_preimage_ref ON utxo_preimages(output_ref)"

-- | Enqueue a single transaction. Computes SHA256 of the JSON payload as the
-- transaction hash, stores L2 addresses for indexed lookup, and returns the hash.
enqueueTxDb ∷ FilePath → QueuedTx → [Text] → IO Text
enqueueTxDb dbPath qtx addrs = withConn dbPath $ \conn →
  withTransaction conn $ do
    now ← getCurrentTime
    let payloadBytes = toStrict (encode qtx)
        txHash = decodeUtf8 . toStrict . encode $ hHash (txId (qtTransaction qtx))
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

-- | Atomically dequeue up to @n@ pending transactions.
-- Returns however many are available (possibly fewer than @n@, including zero).
-- Marks dequeued txs as 'processing'.
dequeueAvailableTxsDb ∷ FilePath → Natural → IO [(Int64, QueuedTx)]
dequeueAvailableTxsDb dbPath n = withConn dbPath $ \conn →
  withExclusiveTransaction conn $ do
    rows ∷ [(Int64, Text)] ←
      query
        conn
        "SELECT id, payload FROM txs WHERE status='pending' ORDER BY id LIMIT ?"
        (Only (fromIntegral n ∷ Int))
    let decoded = sequence [eitherDecodeStrict (encodeUtf8 payload) | (_, payload) ← rows]
    case decoded of
      Left _ → return []
      Right qtxs → do
        forM_ rows $ \(rowId, _) →
          execute
            conn
            "UPDATE txs SET status='processing' WHERE id=?"
            (Only rowId)
        return (zip (map fst rows) qtxs)

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

-- | Revert all 'processing' transactions back to 'pending'.
-- Used after detecting an external state update that may have invalidated
-- in-flight batch computations.
revertProcessingTxsDb ∷ FilePath → IO ()
revertProcessingTxsDb dbPath = withConn dbPath $ \conn →
  execute_ conn "UPDATE txs SET status='pending', batch_id=NULL WHERE status='processing'"

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

-- | Return all pending transactions with their database IDs and parsed payloads.
getPendingTxsWithIdsDb ∷ FilePath → IO [(Int64, QueuedTx)]
getPendingTxsWithIdsDb dbPath = withConn dbPath $ \conn → do
  rows ∷ [(Int64, Text)] ←
    query_
      conn
      "SELECT id, payload FROM txs WHERE status='pending' ORDER BY id"
  return $ catMaybes $ map (\(tid, payload) → case eitherDecodeStrict (encodeUtf8 payload) of
    Right qtx → Just (tid, qtx)
    Left _ → Nothing) rows

-- | Mark specific transactions as 'failed'.
failTxsDb ∷ FilePath → [Int64] → IO ()
failTxsDb dbPath ids = withConn dbPath $ \conn →
  withTransaction conn $
    forM_ ids $ \tid →
      execute
        conn
        "UPDATE txs SET status='failed' WHERE id=?"
        (Only tid)

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

-- ---------------------------------------------------------------------------
-- Ledger state persistence
-- ---------------------------------------------------------------------------

-- | Persist ledger state and leaf hashes to the database (single-row upsert).
-- Called by ChainSync after each rollup state update on-chain.
saveState ∷ FilePath → State I → Leaves Ud (FieldElement I) → IO ()
saveState dbPath ledgerState leafHashes = withConn dbPath $ \conn →
  execute
    conn
    "INSERT OR REPLACE INTO ledger_state (id, ledger_state, utxo_preimage) VALUES (1, ?, ?)"
    (toText ledgerState, toText leafHashes)
 where
  toText ∷ ToJSON a ⇒ a → Text
  toText = decodeUtf8 . toStrict . encode

-- | Load persisted state. Handles migration from the old format (UTxO preimage)
-- to the new format (leaf hashes) by converting if necessary.
loadState ∷ FilePath → IO (Maybe PersistedState)
loadState dbPath = withConn dbPath $ \conn → do
  rows ∷ [(Text, Text)] ←
    query_ conn "SELECT ledger_state, utxo_preimage FROM ledger_state WHERE id = 1"
  case rows of
    [(stText, dataText)] →
      case eitherDecodeStrict (encodeUtf8 stText) of
        Left _ → return Nothing
        Right st →
          -- Try new format: leaf hashes (array of FieldElement values).
          case eitherDecodeStrict (encodeUtf8 dataText) of
            Right leafHashes → return (Just (PersistedState st leafHashes))
            Left _ →
              -- Try old format: full UTxO preimage, convert to leaf hashes.
              case eitherDecodeStrict (encodeUtf8 dataText) of
                Right (preimage ∷ Leaves Ud (UTxO A I)) →
                  return (Just (PersistedState st (fmap (hHash . hash) preimage)))
                Left _ → return Nothing
    _ → return Nothing

-- ---------------------------------------------------------------------------
-- Preimage DB: hash → UTxO mapping
-- ---------------------------------------------------------------------------

-- | Store UTxO preimages in the database, keyed by their leaf hash.
-- This is the Batcher → ChainSync communication channel: the Batcher stores
-- preimages for UTxOs it creates; ChainSync (or rather, the next Batcher
-- iteration) looks them up to reconstruct the preimage vector.
-- Append-only: a UTxO's hash never changes, so entries never need updating.
savePreimagesDb ∷ FilePath → [(FieldElement I, OutputRef I, UTxO A I)] → IO ()
savePreimagesDb dbPath entries = withConn dbPath $ \conn →
  withTransaction conn $
    forM_ entries $ \(leafHash, ref, utxo) →
      execute
        conn
        "INSERT OR IGNORE INTO utxo_preimages (leaf_hash, output_ref, utxo_data) VALUES (?, ?, ?)"
        (toText leafHash, toText ref, toText utxo)
 where
  toText ∷ ToJSON a ⇒ a → Text
  toText = decodeUtf8 . toStrict . encode

-- | Look up UTxO preimages by their leaf hashes.
-- Returns a map from hash text → UTxO for all hashes found in the DB.
lookupPreimagesDb ∷ FilePath → [Text] → IO (Map.Map Text (UTxO A I))
lookupPreimagesDb dbPath hashTexts = withConn dbPath $ \conn → do
  pairs ← forM hashTexts $ \ht → do
    rows ∷ [Only Text] ←
      query conn "SELECT utxo_data FROM utxo_preimages WHERE leaf_hash = ?" (Only ht)
    case rows of
      [Only utxoText] → case eitherDecodeStrict (encodeUtf8 utxoText) of
        Right utxo → return (Just (ht, utxo))
        Left _ → return Nothing
      _ → return Nothing
  return $ Map.fromList (catMaybes pairs)

-- | Look up UTxO preimages by their output refs.
-- Used by 'enqueueTx' to resolve input addresses.
lookupPreimagesByRefDb ∷ FilePath → [Text] → IO [UTxO A I]
lookupPreimagesByRefDb dbPath refTexts = withConn dbPath $ \conn → do
  results ← forM refTexts $ \rt → do
    rows ∷ [Only Text] ←
      query conn "SELECT utxo_data FROM utxo_preimages WHERE output_ref = ?" (Only rt)
    case rows of
      [Only utxoText] → case eitherDecodeStrict (encodeUtf8 utxoText) of
        Right utxo → return (Just utxo)
        Left _ → return Nothing
      _ → return Nothing
  return (catMaybes results)

-- | Return all known output refs from the preimage DB.
-- Used by 'revalidatePendingTxs' to check which inputs are still valid.
getKnownRefsDb ∷ FilePath → IO [Text]
getKnownRefsDb dbPath = withConn dbPath $ \conn → do
  rows ∷ [Only Text] ← query_ conn "SELECT output_ref FROM utxo_preimages"
  return [ref | Only ref ← rows]

-- | One-time migration: seed the preimage DB from the old-format persisted state.
-- If the @utxo_preimage@ column contains a full UTxO preimage vector (old format),
-- extracts each non-null UTxO and inserts it into the @utxo_preimages@ table.
-- After the first 'saveState' call with the new format (leaf hashes), this
-- becomes a no-op since the old format won't parse.
seedPreimageDbFromOldState ∷ FilePath → IO ()
seedPreimageDbFromOldState dbPath = withConn dbPath $ \conn → do
  rows ∷ [Only Text] ←
    query_ conn "SELECT utxo_preimage FROM ledger_state WHERE id = 1"
  case rows of
    [Only dataText] →
      case eitherDecodeStrict (encodeUtf8 dataText) of
        Right (preimage ∷ Leaves Ud (UTxO A I)) →
          withTransaction conn $
            forM_ (fromVector preimage) $ \utxo →
              when (utxo /= nullUTxO) $
                execute
                  conn
                  "INSERT OR IGNORE INTO utxo_preimages (leaf_hash, output_ref, utxo_data) VALUES (?, ?, ?)"
                  (toText (hHash (hash utxo) ∷ FieldElement I), toText (uRef utxo), toText utxo)
        Left _ → return () -- Already in new format or empty.
    _ → return ()
 where
  toText ∷ ToJSON a ⇒ a → Text
  toText = decodeUtf8 . toStrict . encode
