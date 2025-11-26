module ZkFold.Cardano.Rollup.Test.Update (
  rollupUpdateTests,
) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.ByteString (ByteString)
import GeniusYield.Imports ((&))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Algebra.Class (Zero (..))
import ZkFold.Cardano.Rollup.Api
import ZkFold.Cardano.Rollup.Api.Utils (stateToRollupState)
import ZkFold.Cardano.Rollup.Types
import ZkFold.Cardano.Rollup.Utils (proofToPlutus)
import ZkFold.Protocol.NonInteractiveProof (powersOfTauSubset)
import ZkFold.Protocol.Plonkup.Prover (PlonkupProverSecret (..))
import ZkFold.Symbolic.Ledger.Circuit.Compile (
  LedgerContractInput (..),
  ledgerCircuit,
  ledgerProof,
  ledgerSetup,
  mkProof,
  mkSetup,
 )
import ZkFold.Symbolic.Ledger.Examples.One (A, Bi, Bo, I, Ixs, Oxs, TxCount, Ud, batch, newState, prevState, witness)

rollupUpdateTests ∷ Setup → TestTree
rollupUpdateTests setup =
  testGroup
    "rollupUpdateTests"
    [ testCaseSteps "Seed and update rollup state for a simple case" $ \info → withSetup info setup $ \ctx → do
        ts ← powersOfTauSubset
        let lci =
              LedgerContractInput
                { lciTransactionBatch = batch
                , lciStateWitness = witness
                , lciPreviousState = prevState
                , lciNewState = newState
                }
            compiledCircuit = ledgerCircuit @Bi @Bo @Ud @A @Ixs @Oxs @TxCount @I
            proverSecret = PlonkupProverSecret (pure zero)
            setupB = ledgerSetup @ByteString @Bi @Bo @Ud @A @Ixs @Oxs @TxCount @I ts compiledCircuit & mkSetup
            proofB = ledgerProof @ByteString ts proverSecret compiledCircuit lci & mkProof
            fundUser = ctxUserF ctx
            rollupState0 = stateToRollupState prevState
        (initializedBuildInfo, txBodySeed) ← ctxRunBuilder ctx fundUser $ seedRollup setupB Nothing rollupState0
        tidSeed ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodySeed
        info $ "Seed rollup transaction submitted: " <> show tidSeed
        info $ "State NFT: " <> show (zkirbiNFT initializedBuildInfo)
        let proofBPlutus = proofToPlutus proofB
        txBodyUpdate ←
          ctxRunBuilder ctx fundUser $
            runReaderT (updateRollupState rollupState0 proofBPlutus >>= buildTxBody) initializedBuildInfo
        tidUpdate ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyUpdate
        info $ "Update rollup transaction submitted: " <> show tidUpdate
    ]