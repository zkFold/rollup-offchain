{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ZkFold.Cardano.Rollup.Test.Update (
  rollupUpdateTests,
) where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import GHC.Generics (U1 (..), (:*:) (..))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types (unsafeAddressFromText, valueFromLovelace)
import PlutusTx.Builtins qualified as PlutusTx
import System.Environment (getEnv)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (testCaseSteps)
import ZkFold.Algebra.Polynomial.Univariate (PolyVec)
import ZkFold.Protocol.Halo2.Export (runProver)
import ZkFold.Symbolic.Data.Class (arithmetize, payload)
import ZkFold.Symbolic.Interpreter (runInterpreter)
import ZkFold.Symbolic.Ledger.Circuit.Compile (
  LedgerCircuitGates,
  LedgerContractInput (..),
  ledgerCircuit,
 )
import ZkFold.Symbolic.Ledger.Examples.One (
  A,
  Bi,
  Bo,
  G,
  I,
  N,
  S,
  TxCount,
  Ud,
  address,
  batch,
  batch2,
  bridgedIn,
  bridgedIn2,
  newState,
  newState2,
  prevState,
  witness,
  witness2,
 )
import ZkFold.Symbolic.Ledger.Types.Field (RollupBF)

import ZkFold.Cardano.Rollup.Api
import ZkFold.Cardano.Rollup.Api.Utils (computeDelta, stateToRollupState)
import ZkFold.Cardano.Rollup.Types

lci =
  LedgerContractInput
    { lciTransactionBatch = batch
    , lciStateWitness = witness
    , lciPreviousState = prevState
    , lciNewState = newState
    }

compiledCircuit = ledgerCircuit @Bi @Bo @Ud @A @S @N @TxCount @I

witnessInputs1 = runInterpreter $ arithmetize lci

compiledInput1 = (witnessInputs1 :*: U1) :*: (payload lci :*: U1)

rollupState0 = stateToRollupState prevState

rollupState1 = stateToRollupState newState

lci2 =
  LedgerContractInput
    { lciTransactionBatch = batch2
    , lciStateWitness = witness2
    , lciPreviousState = newState
    , lciNewState = newState2
    }

witnessInputs2 = runInterpreter $ arithmetize lci2

compiledInput2 = (witnessInputs2 :*: U1) :*: (payload lci2 :*: U1)

rollupState2 = stateToRollupState newState2

delta1 = computeDelta @Bi @Bo @Ud @A @S @N @TxCount witness batch bridgedIn newState

delta2 = computeDelta @Bi @Bo @Ud @A @S @N @TxCount witness2 batch2 bridgedIn2 newState2

rollupUpdateTests ∷ Setup → TestTree
rollupUpdateTests setup =
  withResource
    ( do
        proverExe ← getEnv "HALO2_PROVER"
        Right proofB ←
          runExceptT $ runProver @_ @_ @LedgerCircuitGates @_ @(PolyVec RollupBF) proverExe compiledCircuit compiledInput1
        Right proofB2 ←
          runExceptT $ runProver @_ @_ @LedgerCircuitGates @_ @(PolyVec RollupBF) proverExe compiledCircuit compiledInput2
        pure (proofB, proofB2)
    )
    (\_ → pure ())
    $ \getResources →
      testGroup
        "rollupUpdateTests"
        [ testCaseSteps "Seed and update rollup state for a simple case" $ \info → withSetup info setup $ \ctx → do
            (proofB, proofB2) ← getResources
            let
              fundUser = ctxUserF ctx
            (initializedBuildInfo, txBodySeed) ← ctxRunBuilder ctx fundUser $ seedRollup 1 1 1 Nothing rollupState0
            tidSeed ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodySeed
            info $ "Seed rollup transaction submitted: " <> show tidSeed
            info $ "State NFT: " <> show (zkirbiNFT initializedBuildInfo)
            txBodyRegisterStake ← ctxRunBuilder ctx fundUser $ runReaderT (registerRollupStake >>= buildTxBody) initializedBuildInfo
            tidRegisterStake ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyRegisterStake
            info $ "Register stake transaction submitted: " <> show tidRegisterStake
            let proofBPlutus = PlutusTx.toBuiltin proofB
            txBodyUpdate ←
              ctxRunBuilder ctx fundUser $
                runReaderT
                  (updateRollupState rollupState1 [(valueFromLovelace 5_000_000, address)] [] proofBPlutus delta1 >>= buildTxBody)
                  initializedBuildInfo
            tidUpdate ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyUpdate
            info $ "Update rollup transaction submitted: " <> show tidUpdate
            info "Posting another update rollup, which bridges out a value"
            let proofB2Plutus = PlutusTx.toBuiltin proofB2
            txBodyUpdate2 ←
              ctxRunBuilder ctx fundUser $
                runReaderT
                  ( updateRollupState
                      rollupState2
                      []
                      [
                        ( valueFromLovelace 5_000_000
                        , unsafeAddressFromText
                            "addr_test1qpxsldf6hmp5vtdhhwzukm8x5q0m9t2xh8cftx8s6a43vll3t8hyc5syfx9lltq9dgr2xdkvwahr9humhpa9tae2jcjsxpxw2h"
                        )
                      ]
                      proofB2Plutus
                      delta2
                      >>= buildTxBody
                  )
                  initializedBuildInfo
            tidUpdate2 ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyUpdate2
            info $ "Update rollup transaction submitted which bridges out a value: " <> show tidUpdate2
            -- , testCaseSteps "Rollup update where bridge in happens separately" $ \info → withSetup info setup $ \ctx → do
            --     (_ts, setupB, proofB, proofB2) ← getResources
            --     let
            --       fundUser = ctxUserF ctx
            --     (initializedBuildInfo, txBodySeed) ← ctxRunBuilder ctx fundUser $ seedRollup setupB 1 1 1 Nothing rollupState0
            --     tidSeed ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodySeed
            --     info $ "Seed rollup transaction submitted: " <> show tidSeed
            --     info $ "State NFT: " <> show (zkirbiNFT initializedBuildInfo)
            --     txBodyRegisterStake ← ctxRunBuilder ctx fundUser $ runReaderT (registerRollupStake >>= buildTxBody) initializedBuildInfo
            --     tidRegisterStake ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyRegisterStake
            --     info $ "Register stake transaction submitted: " <> show tidRegisterStake
            --     txBodyBridgeIn ←
            --       ctxRunBuilder ctx fundUser $
            --         runReaderT
            --           (bridgeIn [(address, valueFromLovelace 5_000_000)] >>= buildTxBody)
            --           initializedBuildInfo
            --     tidBridgeIn ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyBridgeIn
            --     info $ "Bridge in transaction submitted: " <> show tidBridgeIn
            --     let proofBPlutus = proofToPlutus proofB
            --     txBodyUpdate ←
            --       ctxRunBuilder ctx fundUser $
            --         runReaderT
            --           (updateRollupState rollupState1 [] [] proofBPlutus delta1 >>= buildTxBody)
            --           initializedBuildInfo
            --     tidUpdate ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyUpdate
            --     info $ "Update rollup transaction submitted: " <> show tidUpdate
            --     info "Posting another update rollup, which bridges out a value"
            --     let proofB2Plutus = proofToPlutus proofB2
            --     txBodyUpdate2 ←
            --       ctxRunBuilder ctx fundUser $
            --         runReaderT
            --           ( updateRollupState
            --               rollupState2
            --               []
            --               [
            --                 ( valueFromLovelace 5_000_000
            --                 , unsafeAddressFromText
            --                     "addr_test1qpxsldf6hmp5vtdhhwzukm8x5q0m9t2xh8cftx8s6a43vll3t8hyc5syfx9lltq9dgr2xdkvwahr9humhpa9tae2jcjsxpxw2h"
            --                 )
            --               ]
            --               proofB2Plutus
            --               delta2
            --               >>= buildTxBody
            --           )
            --           initializedBuildInfo
            --     tidUpdate2 ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyUpdate2
            --     info $ "Update rollup transaction submitted which bridges out a value: " <> show tidUpdate2
        ]
