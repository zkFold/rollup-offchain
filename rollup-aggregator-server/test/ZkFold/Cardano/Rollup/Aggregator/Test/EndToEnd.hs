{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.Rollup.Aggregator.Test.EndToEnd (endToEndTests) where

import Control.Concurrent.STM (atomically)
import Control.Monad.Reader (runReaderT)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Data (Proxy (..))
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import GHC.Generics ((:.:) (..), type (:*:) (..))
import GHC.Natural (Natural)
import GHC.TypeNats (natVal)
import GeniusYield.Test.FakeCoin (FakeCoin (..), fakePolicy, fakeValue)
import GeniusYield.Test.Privnet.Ctx (
  ctxNetworkId,
  ctxProviders,
  ctxRun,
  ctxRunBuilder,
  ctxRunQuery,
  ctxUserF,
  ctxWaitNextBlock,
 )
import GeniusYield.Test.Privnet.Setup (Setup, withSetup)
import GeniusYield.TxBuilder (buildTxBody, mustMint, signAndSubmitConfirmed, userAddr, userPaymentSKey', utxosAtAddress)
import GeniusYield.Types (
  GYBuildPlutusScript (GYBuildPlutusScriptInlined),
  GYBuildScript (GYBuildPlutusScript),
  GYSomePaymentSigningKey (AGYPaymentSigningKey),
  PlutusVersion (PlutusV2),
  addressToBech32,
  gySubmitTx,
  signGYTx,
  unitRedeemer,
  unsafeAddressFromText,
  utxoRef,
  utxosToList,
  valueFromLovelace,
 )
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCaseSteps)
import ZkFold.Algebra.Class (FromConstant (..), ToConstant (..))
import ZkFold.Cardano.Rollup.Aggregator.Batcher (initBatcherState, processBatch, takeExactly)
import ZkFold.Cardano.Rollup.Aggregator.Config (BatchConfig (..))
import ZkFold.Cardano.Rollup.Aggregator.Ctx qualified as AggCtx
import ZkFold.Cardano.Rollup.Aggregator.Handlers (handleBridgeIn, handleQueryL2Utxos, handleSubmitTx)
import ZkFold.Cardano.Rollup.Aggregator.Types (
  BridgeInRequest (..),
  BridgeInResponse (..),
  QueryL2UtxosResponse (..),
  SubmitTxRequest (..),
  SubmitTxResponse (..),
 )
import ZkFold.Cardano.Rollup.Api (registerRollupStake, seedRollup)
import ZkFold.Cardano.Rollup.Api.Utils (stateToRollupState)
import ZkFold.Data.Vector (fromVector)
import ZkFold.Symbolic.Ledger.Circuit.Compile (ledgerSetup, mkSetup)
import ZkFold.Symbolic.Ledger.Examples.Three qualified as Ex3
import ZkFold.Symbolic.Ledger.Types (Output (..), Transaction (..), UTxO (..), adaName, adaPolicy)
import ZkFold.Symbolic.Ledger.Types.Value (AssetValue (..))
import ZkFold.Symbolic.Ledger.Utils (unsafeToVector')

endToEndTests ∷ Setup → TestTree
endToEndTests setup =
  withResource
    ( do
        (queue, stateVar, utxoVar, ts, circuit, proverSecret) ← initBatcherState Nothing
        setupBytesJson ← BSL.readFile "rollup-aggregator-server/test/data/setup-bytes.json"
        let setupBytes =
              -- ledgerSetup @ByteString @Ex3.Bi @Ex3.Bo @Ex3.Ud @Ex3.A @Ex3.Ixs @Ex3.Oxs @Ex3.TxCount @Ex3.I ts circuit
              --   & mkSetup
              fromMaybe undefined (Aeson.decode setupBytesJson)
        pure (queue, stateVar, utxoVar, ts, circuit, proverSecret, setupBytes)
    )
    (\_ → pure ())
    $ \getResources →
      testGroup
        "End-to-end tests"
        [ testCaseSteps "Bridge-in + L2 txs + batch processing" $ \info → withSetup info setup $ \privCtx → do
            (queue, stateVar, utxoVar, ts, circuit, proverSecret, setupBytes) ← getResources
            -- BSL.writeFile "rollup-aggregator-server/test/data/setup-bytes.json" (Aeson.encode setupBytes)
            -- Step 1: Admin setup — seed rollup and register stake validator
            let fundUser = ctxUserF privCtx
                rollupState0 = stateToRollupState Ex3.prevState

            (buildInfo, txBodySeed) ←
              ctxRunBuilder privCtx fundUser $
                seedRollup setupBytes 1 1 2 Nothing rollupState0
            tidSeed ← ctxRun privCtx fundUser $ signAndSubmitConfirmed txBodySeed
            info $ "Seed rollup: " <> show tidSeed

            txBodyRegStake ←
              ctxRunBuilder privCtx fundUser $
                runReaderT (registerRollupStake >>= buildTxBody) buildInfo
            tidRegStake ← ctxRun privCtx fundUser $ signAndSubmitConfirmed txBodyRegStake
            info $ "Register stake: " <> show tidRegStake

            -- Step 2: Mint asset2 (FakeCoin "zk-rollup") — deterministic policy hash matching Ex3.asset2Policy
            let asset2 = FakeCoin "zk-rollup"
                mintPolicy = fakePolicy asset2
                mintSkel = mustMint @'PlutusV2 (GYBuildPlutusScript (GYBuildPlutusScriptInlined mintPolicy)) unitRedeemer "zk-rollup" 50_000_000
            txBodyMint ← ctxRunBuilder privCtx fundUser $ buildTxBody mintSkel
            tidMint ← ctxRun privCtx fundUser $ signAndSubmitConfirmed txBodyMint
            info $ "Mint asset2: " <> show tidMint
            ctxWaitNextBlock privCtx

            -- Step 3: Create aggregator Ctx
            let nid = ctxNetworkId privCtx
                providers = ctxProviders privCtx
            userUtxos ← ctxRunQuery privCtx $ utxosAtAddress (userAddr fundUser) Nothing
            let collateralRef = utxoRef $ head $ utxosToList userUtxos
                aggCtx =
                  AggCtx.Ctx
                    { AggCtx.ctxNetworkId = nid
                    , AggCtx.ctxProviders = providers
                    , AggCtx.ctxSigningKey = (AGYPaymentSigningKey (userPaymentSKey' fundUser), userAddr fundUser)
                    , AggCtx.ctxCollateral = collateralRef
                    , AggCtx.ctxRollupBuildInfo = buildInfo
                    , AggCtx.ctxBatchConfig = BatchConfig {bcBatchTransactions = 2, bcBatchIntervalSeconds = 60}
                    , AggCtx.ctxBatchQueue = queue
                    , AggCtx.ctxLedgerStateVar = stateVar
                    , AggCtx.ctxUtxoPreimageVar = utxoVar
                    , AggCtx.ctxTrustedSetup = ts
                    , AggCtx.ctxLedgerCircuit = circuit
                    , AggCtx.ctxProverSecret = proverSecret
                    , AggCtx.ctxStatePersistPath = "/tmp/rollup-aggregator-test-state.json"
                    }

            -- Step 4: Bridge-in via handleBridgeIn (10 ADA + 50 asset2)
            let bridgeInValue = valueFromLovelace 10_000_000 <> fakeValue asset2 50_000_000
                birReq =
                  BridgeInRequest
                    { birAmount = bridgeInValue
                    , birDestinationAddress = Right Ex3.address
                    , birUsedAddresses = [addressToBech32 (userAddr fundUser)]
                    , birChangeAddress = addressToBech32 (userAddr fundUser)
                    }
            BridgeInResponse unsignedTx ← handleBridgeIn aggCtx birReq
            info "Bridge-in tx built"

            -- Step 5: Sign and submit bridge-in tx
            let signedTx = signGYTx unsignedTx [userPaymentSKey' fundUser]
            _bridgeInTxId ← gySubmitTx providers signedTx
            ctxWaitNextBlock privCtx
            info "Bridge-in tx submitted and confirmed"

            -- Step 6: Submit L2 txs via handleSubmitTx
            -- Ex3.sigs is batch-level (Vector TxCount :.: Vector Ixs :.: ...), extract per-tx signatures
            let perTxSigs = fromVector $ unComp1 Ex3.sigs
                perTxSigs2 = fromVector $ unComp1 Ex3.sigs2

            -- tx1: no bridge-outs
            let strReq1 =
                  SubmitTxRequest
                    { strTransaction = Ex3.tx1
                    , strSignatures = head perTxSigs
                    , strBridgeOuts = []
                    }
            SubmitTxResponse status1 ← handleSubmitTx aggCtx strReq1
            assertEqual "L2 tx1 queued" "queued" status1
            info "L2 tx1 queued"

            -- tx2: no bridge-outs
            let strReq2 =
                  SubmitTxRequest
                    { strTransaction = Ex3.tx2
                    , strSignatures = perTxSigs !! 1
                    , strBridgeOuts = []
                    }
            SubmitTxResponse status2 ← handleSubmitTx aggCtx strReq2
            assertEqual "L2 tx2 queued" "queued" status2
            info "L2 tx2 queued"

            let txCount = natVal (Proxy @Ex3.TxCount)
            queuedTxs ← atomically $ takeExactly txCount queue
            case queuedTxs of
              Nothing → assertFailure "No transactions in batch queue"
              Just txs → do
                tid ← processBatch aggCtx txs
                info $ "Batch submitted: " <> show tid

            -- Step 6b: Query L2 UTxOs after batch 1
            -- After tx1+tx2: address has 5 ADA + 25 asset2, address2 has 5 ADA + 25 asset2
            let [outTx3_1 :*: _, _] = Ex3.tx3 & outputs & unComp1 & fromVector
            QueryL2UtxosResponse utxos1Addr ← handleQueryL2Utxos aggCtx Ex3.address
            assertEqual "UTxO count at address after batch 1" 1 (length utxos1Addr)
            assertEqual "UTxO output at address after batch 1" outTx3_1 (uOutput (head utxos1Addr))
            info "UTxOs at address after batch 1: verified"

            QueryL2UtxosResponse utxos1Addr2 ← handleQueryL2Utxos aggCtx Ex3.address2
            assertEqual "UTxO count at address2 after batch 1" 1 (length utxos1Addr2)
            assertEqual "UTxO output at address2 after batch 1" (outTx3_1 {oAddress = Ex3.address2}) (uOutput (head utxos1Addr2))
            info "UTxOs at address2 after batch 1: verified"

            -- tx3: 1 bridge-out (5 ADA + 25 asset2 to bridge-out address)
            let bridgeOutAddr =
                  addressToBech32 $
                    unsafeAddressFromText
                      "addr_test1qpxsldf6hmp5vtdhhwzukm8x5q0m9t2xh8cftx8s6a43vll3t8hyc5syfx9lltq9dgr2xdkvwahr9humhpa9tae2jcjsxpxw2h"
                bridgeOutValue = valueFromLovelace 5_000_000 <> fakeValue asset2 25_000_000
                strReq3 =
                  SubmitTxRequest
                    { strTransaction = Ex3.tx3
                    , strSignatures = head perTxSigs2
                    , strBridgeOuts = [(bridgeOutValue, bridgeOutAddr)]
                    }
            SubmitTxResponse status3 ← handleSubmitTx aggCtx strReq3
            assertEqual "L2 tx3 queued" "queued" status3
            info "L2 tx3 queued"

            -- tx4: no bridge-outs
            let strReq4 =
                  SubmitTxRequest
                    { strTransaction = Ex3.tx4
                    , strSignatures = perTxSigs2 !! 1
                    , strBridgeOuts = []
                    }
            SubmitTxResponse status4 ← handleSubmitTx aggCtx strReq4
            assertEqual "L2 tx4 queued" "queued" status4
            info "L2 tx4 queued"

            queuedTxs2 ← atomically $ takeExactly txCount queue
            case queuedTxs2 of
              Nothing → assertFailure "No transactions in batch queue"
              Just txs → do
                tid ← processBatch aggCtx txs
                info $ "Batch submitted: " <> show tid

            -- Step 8b: Query L2 UTxOs after batch 2
            let [out1 :*: _, out2 :*: _] = Ex3.tx4 & outputs & unComp1 & fromVector
            QueryL2UtxosResponse utxos2Addr ← handleQueryL2Utxos aggCtx Ex3.address
            assertEqual "UTxO count at address after batch 2" 1 (length utxos2Addr)
            assertEqual "UTxO output at address after batch 2" out1 (uOutput (head utxos2Addr))
            info "UTxOs at address after batch 2: verified"

            QueryL2UtxosResponse utxos2Addr2 ← handleQueryL2Utxos aggCtx Ex3.address2
            assertEqual "UTxO count at address2 after batch 2" 1 (length utxos2Addr2)
            assertEqual
              "UTxO output at address2 after batch 2"
              out2
              (uOutput (head utxos2Addr2))
            info "UTxOs at address2 after batch 2: verified"

            info "End-to-end test passed"
        ]
