module ZkFold.Cardano.Rollup.Api.Utils (
  feToInteger,
  stateToRollupState,
) where

import Data.Function ((&))
import ZkFold.Algebra.Class (PrimeField (..), ToConstant (..))
import ZkFold.Cardano.UPLC.RollupSimple.Types
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hash (hHash))
import ZkFold.Symbolic.Data.MerkleTree (MerkleTree (mHash))
import ZkFold.Symbolic.Interpreter (Interpreter)
import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Types.State

-- | Convert a field element to an 'Integer'.
feToInteger ∷ (PrimeField a, IntegralOf a ~ Integer) ⇒ FieldElement (Interpreter a) → Integer
feToInteger = toIntegral . toConstant

-- | Symbolic 'State' to Plutus 'RollupState'.
stateToRollupState ∷ ∀ bi bo ud a. State bi bo ud a RollupBFInterpreter → RollupState
stateToRollupState State {..} =
  RollupState
    { utxoTreeRoot = sUTxO & mHash & feToInteger
    , previousStateHash = feToInteger sPreviousStateHash
    , chainLength = feToInteger sLength
    , bridgeOutCommitment = sBridgeOut & hHash & feToInteger
    , bridgeInCommitment = sBridgeIn & hHash & feToInteger
    }