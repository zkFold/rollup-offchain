module ZkFold.Cardano.Rollup.Aggregator.Persistence (
  PersistedState (..),
  initDb,
  enqueueTxDb,
  dequeueTxsDb,
  saveState,
  loadState,
) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString.Lazy (toStrict)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple
import Deriving.Aeson
import GHC.Natural (Natural)
import GeniusYield.Types (LowerFirst)
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

-- | Enqueue a single transaction by inserting its JSON payload into the DB.
enqueueTxDb ∷ FilePath → QueuedTx → IO ()
enqueueTxDb dbPath qtx = withConn dbPath $ \conn →
  execute conn "INSERT INTO pending_txs (payload) VALUES (?)" (Only (toText qtx))
 where
  toText ∷ QueuedTx → Text
  toText = decodeUtf8 . toStrict . encode

-- | Atomically dequeue exactly @n@ transactions.
-- Returns 'Nothing' if fewer than @n@ are available (rows are NOT deleted).
-- Returns 'Just' the decoded transactions and deletes those rows otherwise.
dequeueTxsDb ∷ FilePath → Natural → IO (Maybe [QueuedTx])
dequeueTxsDb dbPath n = withConn dbPath $ \conn →
  withExclusiveTransaction conn $ do
    rows ∷ [(Int64, Text)] ←
      query
        conn
        "SELECT id, payload FROM pending_txs ORDER BY id LIMIT ?"
        (Only (fromIntegral n ∷ Int))
    if length rows < fromIntegral n
      then return Nothing
      else do
        let decoded = sequence [eitherDecodeStrict (encodeUtf8 payload) | (_, payload) ← rows]
        case decoded of
          Left _ → return Nothing
          Right qtxs → do
            forM_ rows $ \(rowId, _) →
              execute conn "DELETE FROM pending_txs WHERE id = ?" (Only rowId)
            return (Just qtxs)

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
