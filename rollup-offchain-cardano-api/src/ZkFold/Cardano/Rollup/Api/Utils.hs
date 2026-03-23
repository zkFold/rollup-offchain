module ZkFold.Cardano.Rollup.Api.Utils (
  feToInteger,
  stateToRollupState,
  computeDelta,
) where

import Data.Function (($), (&))
import Data.Functor (fmap)
import GHC.Generics ((:*:) (..), (:.:) (..))
import GHC.TypeNats (KnownNat, type (-))
import ZkFold.Algebra.Class (FromConstant (..), PrimeField (..), ToConstant (..))
import ZkFold.Cardano.UPLC.RollupSimple.Types
import ZkFold.Data.MerkleTree (MerkleTreeSize)
import ZkFold.Data.Vector (Vector, fromVector)
import ZkFold.Symbolic.Class (Symbolic)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hash (hHash), hash)
import ZkFold.Symbolic.Data.MerkleTree (packIndex)
import ZkFold.Symbolic.Data.MerkleTree qualified as SymMerkle
import ZkFold.Symbolic.Interpreter (Interpreter)
import ZkFold.Symbolic.Data.Bool (BoolType (false))
import ZkFold.Symbolic.Data.Bool qualified as ZkBool
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field (RollupBF, RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Validation.State (StateWitness (..))
import ZkFold.Symbolic.Ledger.Validation.Transaction (TransactionWitness (..))
import ZkFold.Symbolic.Ledger.Validation.TransactionBatch (TransactionBatchWitness (..))
import Prelude (Bool, Integer, Num (..), concatMap, map, not, zip, zip3, (&&), (==), (/=), (.), (<>))

-- | Convert a field element to an 'Integer'.
feToInteger ∷ (PrimeField a, IntegralOf a ~ Integer) ⇒ FieldElement (Interpreter a) → Integer
feToInteger = toIntegral . toConstant

-- | Symbolic 'State' to Plutus 'RollupState'.
stateToRollupState ∷ State RollupBFInterpreter → RollupState
stateToRollupState State {..} =
  RollupState
    { utxoTreeRoot = sUTxO & feToInteger
    , previousStateHash = feToInteger sPreviousStateHash
    , chainLength = feToInteger sLength
    }

-- | Extract the tree delta from the state witness and transaction data.
-- Produces a flat list of integers matching the circuit's public output order:
--   [bi*(pos,hash)] ++ [t*n*pos] ++ [t*n*(isActive,pos,hash)]
computeDelta
  ∷ ∀ bi bo ud a s n t
   . ( Symbolic RollupBFInterpreter
     , KnownNat (ud - 1)
     , KnownNat (MerkleTreeSize ud)
     , KnownNat bi, KnownNat bo, KnownNat a, KnownNat s, KnownNat n, KnownNat t
     )
  ⇒ StateWitness bi bo ud a s n t RollupBFInterpreter
  → TransactionBatch n a t RollupBFInterpreter
  → (Vector bi :.: Output a) RollupBFInterpreter
  → State RollupBFInterpreter
  -- ^ New state (for computing bridge-in UTxO refs).
  → [Integer]
computeDelta witness batch bridgedIn newSt =
  let
    fe ∷ FieldElement RollupBFInterpreter → Integer
    fe = feToInteger

    -- Bridge-in delta: (packedPosition, newHash) per bridge-in
    bridgeInHash = sLength newSt & hash & hHash
    biDelta = concatMap
      (\(entry, (output, ix)) →
        let pos = packIndex (SymMerkle.position entry)
            utxo = UTxO {uRef = OutputRef {orTxId = bridgeInHash, orIndex = fromConstant ix}, uOutput = output}
            utxoHash = hash utxo & hHash
         in [fe pos, fe utxoHash]
      )
      (zip (fromVector $ unComp1 $ swAddBridgeIn witness)
           (zip (fromVector $ unComp1 bridgedIn) [(0 ∷ Integer) ..]))

    -- Input delta: packedPosition per input per transaction
    txWitnesses = fromVector $ unComp1 $ tbwTransactions (swTransactionBatch witness)
    inputDelta = concatMap
      (\tw → map
        (\(entry :*: _) → fe $ packIndex (SymMerkle.position entry))
        (fromVector $ unComp1 $ twInputs tw)
      )
      txWitnesses

    -- Output delta: (isActive, packedPosition, newHash) per output per transaction
    txs = fromVector (tbTransactions batch)
    outputDelta = concatMap
      (\(tx, tw) →
        let txId' = txId tx & hHash
         in concatMap
              (\((output :*: bout), entry, ix) →
                let isBout = bout /= (false ∷ ZkBool.Bool RollupBFInterpreter)
                    isNull = output == nullOutput
                    isActive = not isBout && not isNull
                    pos = packIndex (SymMerkle.position entry)
                    utxo = UTxO {uRef = OutputRef {orTxId = txId', orIndex = fromConstant ix}, uOutput = output}
                    utxoHash = hash utxo & hHash
                 in [ if isActive then 1 else 0
                    , fe pos
                    , fe utxoHash
                    ]
              )
              (zip3 (fromVector $ unComp1 $ outputs tx)
                    (fromVector $ unComp1 $ twOutputs tw)
                    [(0 ∷ Integer) ..])
      )
      (zip txs txWitnesses)
   in biDelta <> inputDelta <> outputDelta
