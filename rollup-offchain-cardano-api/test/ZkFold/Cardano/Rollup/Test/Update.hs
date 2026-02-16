{-# OPTIONS_GHC -Wno-missing-signatures #-}

module ZkFold.Cardano.Rollup.Test.Update (
  rollupUpdateTests,
) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.ByteString (ByteString)
import GeniusYield.Imports ((&))
import GeniusYield.Test.Privnet.Ctx
import GeniusYield.Test.Privnet.Setup
import GeniusYield.TxBuilder
import GeniusYield.Types (unsafeAddressFromText, valueFromLovelace)
import Test.Tasty (TestTree, testGroup, withResource)
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
import ZkFold.Symbolic.Ledger.Examples.One (
  A,
  Bi,
  Bo,
  I,
  Ixs,
  Oxs,
  TxCount,
  Ud,
  address,
  batch,
  batch2,
  newState,
  newState2,
  prevState,
  witness,
  witness2,
 )

lci =
  LedgerContractInput
    { lciTransactionBatch = batch
    , lciStateWitness = witness
    , lciPreviousState = prevState
    , lciNewState = newState
    }

compiledCircuit = ledgerCircuit @Bi @Bo @Ud @A @Ixs @Oxs @TxCount @I

proverSecret = PlonkupProverSecret (pure zero)

rollupState0 = stateToRollupState prevState

rollupState1 = stateToRollupState newState

lci2 =
  LedgerContractInput
    { lciTransactionBatch = batch2
    , lciStateWitness = witness2
    , lciPreviousState = newState
    , lciNewState = newState2
    }

rollupState2 = stateToRollupState newState2

rollupUpdateTests ∷ Setup → TestTree
rollupUpdateTests setup =
  withResource
    ( do
        ts ← powersOfTauSubset
        let
          setupB = ledgerSetup @ByteString @Bi @Bo @Ud @A @Ixs @Oxs @TxCount @I ts compiledCircuit & mkSetup
          proofB = ledgerProof @ByteString ts proverSecret compiledCircuit lci & mkProof
          proofB2 = ledgerProof @ByteString ts proverSecret compiledCircuit lci2 & mkProof
        pure (ts, setupB, proofB, proofB2)
    )
    (\_ → pure ())
    $ \getResources →
      testGroup
        "rollupUpdateTests"
        [ testCaseSteps "Seed and update rollup state for a simple case" $ \info → withSetup info setup $ \ctx → do
            (_ts, setupB, proofB, proofB2) ← getResources
            let
              fundUser = ctxUserF ctx
            (initializedBuildInfo, txBodySeed) ← ctxRunBuilder ctx fundUser $ seedRollup setupB 1 1 1 Nothing rollupState0
            tidSeed ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodySeed
            info $ "Seed rollup transaction submitted: " <> show tidSeed
            info $ "State NFT: " <> show (zkirbiNFT initializedBuildInfo)
            txBodyRegisterStake ← ctxRunBuilder ctx fundUser $ runReaderT (registerRollupStake >>= buildTxBody) initializedBuildInfo
            tidRegisterStake ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyRegisterStake
            info $ "Register stake transaction submitted: " <> show tidRegisterStake
            let proofBPlutus = proofToPlutus proofB
            txBodyUpdate ←
              ctxRunBuilder ctx fundUser $
                runReaderT
                  (updateRollupState rollupState1 [(valueFromLovelace 5_000_000, address)] [] proofBPlutus >>= buildTxBody)
                  initializedBuildInfo
            tidUpdate ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyUpdate
            info $ "Update rollup transaction submitted: " <> show tidUpdate
            info "Posting another update rollup, which bridges out a value"
            let proofB2Plutus = proofToPlutus proofB2
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
                      >>= buildTxBody
                  )
                  initializedBuildInfo
            tidUpdate2 ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyUpdate2
            info $ "Update rollup transaction submitted which bridges out a value: " <> show tidUpdate2
        , testCaseSteps "Rollup update where bridge in happens separately" $ \info → withSetup info setup $ \ctx → do
            (_ts, setupB, proofB, proofB2) ← getResources
            let
              fundUser = ctxUserF ctx
            (initializedBuildInfo, txBodySeed) ← ctxRunBuilder ctx fundUser $ seedRollup setupB 1 1 1 Nothing rollupState0
            tidSeed ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodySeed
            info $ "Seed rollup transaction submitted: " <> show tidSeed
            info $ "State NFT: " <> show (zkirbiNFT initializedBuildInfo)
            txBodyRegisterStake ← ctxRunBuilder ctx fundUser $ runReaderT (registerRollupStake >>= buildTxBody) initializedBuildInfo
            tidRegisterStake ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyRegisterStake
            info $ "Register stake transaction submitted: " <> show tidRegisterStake
            txBodyBridgeIn ←
              ctxRunBuilder ctx fundUser $
                runReaderT
                  (bridgeIn' [(address, valueFromLovelace 5_000_000)] >>= buildTxBody)
                  initializedBuildInfo
            tidBridgeIn ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyBridgeIn
            info $ "Bridge in transaction submitted: " <> show tidBridgeIn
            let proofBPlutus = proofToPlutus proofB
            txBodyUpdate ←
              ctxRunBuilder ctx fundUser $
                runReaderT
                  (updateRollupState rollupState1 [] [] proofBPlutus >>= buildTxBody)
                  initializedBuildInfo
            tidUpdate ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyUpdate
            info $ "Update rollup transaction submitted: " <> show tidUpdate
            info "Posting another update rollup, which bridges out a value"
            let proofB2Plutus = proofToPlutus proofB2
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
                      >>= buildTxBody
                  )
                  initializedBuildInfo
            tidUpdate2 ← ctxRun ctx fundUser $ signAndSubmitConfirmed txBodyUpdate2
            info $ "Update rollup transaction submitted which bridges out a value: " <> show tidUpdate2
        ]
