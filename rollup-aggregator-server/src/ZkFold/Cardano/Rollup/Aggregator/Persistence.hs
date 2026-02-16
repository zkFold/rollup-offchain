module ZkFold.Cardano.Rollup.Aggregator.Persistence (
  PersistedState (..),
  saveState,
  loadState,
) where

import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Deriving.Aeson
import GeniusYield.Types (LowerFirst)
import System.Directory (doesFileExist, renameFile)
import ZkFold.Cardano.Rollup.Aggregator.Types
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Symbolic.Ledger.Types

-- | State persisted to disk across server restarts.
data PersistedState = PersistedState
  { psLedgerState ∷ !(State Bi Bo Ud A I)
  , psUtxoPreimage ∷ !(Leaves Ud (UTxO A I))
  }
  deriving stock Generic
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "ps", LowerFirst]] PersistedState

-- | Atomically save state to a file (write to temp, then rename).
saveState ∷ FilePath → State Bi Bo Ud A I → Leaves Ud (UTxO A I) → IO ()
saveState path ledgerState utxoPreimage = do
  let tmpPath = path <> ".tmp"
  encodeFile tmpPath (PersistedState ledgerState utxoPreimage)
  renameFile tmpPath path

-- | Load state from a file. Returns 'Nothing' if the file doesn't exist or is corrupt.
loadState ∷ FilePath → IO (Maybe PersistedState)
loadState path = do
  exists ← doesFileExist path
  if exists
    then do
      result ← eitherDecodeFileStrict path
      case result of
        Left _err → pure Nothing
        Right ps → pure (Just ps)
    else pure Nothing
