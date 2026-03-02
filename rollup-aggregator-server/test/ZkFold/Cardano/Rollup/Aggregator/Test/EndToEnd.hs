{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ZkFold.Cardano.Rollup.Aggregator.Test.EndToEnd (endToEndTests) where

import Control.Monad.Reader (runReaderT)
import Data.Aeson qualified as Aeson
-- import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Data (Proxy (..))
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import GHC.Generics ((:.:) (..), type (:*:) (..))
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
  utxoRef,
  utxosToList,
  valueFromLovelace,
 )
import System.Directory (removePathForcibly)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCaseSteps)
import ZkFold.Cardano.Rollup.Aggregator.Batcher (initBatcherState, processBatch)
import ZkFold.Cardano.Rollup.Aggregator.Config (BatchConfig (..))
import ZkFold.Cardano.Rollup.Aggregator.Ctx qualified as AggCtx
import ZkFold.Cardano.Rollup.Aggregator.Handlers (
  handleBatches,
  handleBridgeIn,
  handleBridgeOuts,
  handleGetBatch,
  handleGetTx,
  handlePendingTxs,
  handleQueryL2Utxos,
  handleSubmitTx,
  handleTxsByAddress,
 )
import ZkFold.Cardano.Rollup.Aggregator.Persistence (dequeueTxsDb, initDb)
import ZkFold.Cardano.Rollup.Aggregator.Types (
  BatchDetailResponse (..),
  BatchesResponse (..),
  BridgeInRequest (..),
  BridgeInResponse (..),
  BridgeOutEntry (..),
  BridgeOutsResponse (..),
  PendingTxsResponse (..),
  QueryL2UtxosResponse (..),
  SubmitTxRequest (..),
  SubmitTxResponse (..),
  TxRecord (..),
  TxResponse (..),
  TxStatus (..),
  TxsByAddressResponse (..),
 )
import ZkFold.Cardano.Rollup.Api (registerRollupStake, seedRollup)
import ZkFold.Cardano.Rollup.Api.Utils (stateToRollupState)
import ZkFold.Data.Vector (fromVector)
-- import ZkFold.Symbolic.Ledger.Circuit.Compile (ledgerSetup, mkSetup)
import ZkFold.Symbolic.Ledger.Examples.Three qualified as Ex3
import ZkFold.Symbolic.Ledger.Types (Output (..), Transaction (..), UTxO (..))

endToEndTests ∷ Setup → TestTree
endToEndTests setup =
  withResource
    ( do
        let dbPath = "/tmp/rollup-aggregator-test.db"
        removePathForcibly dbPath
        initDb dbPath
        batcherState ← initBatcherState dbPath
        setupBytesJson ← BSL.readFile "rollup-aggregator-server/test/data/setup-bytes.json"
        let setupBytes =
              -- ledgerSetup @ByteString @Ex3.Bi @Ex3.Bo @Ex3.Ud @Ex3.A @Ex3.Ixs @Ex3.Oxs @Ex3.TxCount @Ex3.I ts circuit
              --   & mkSetup
              fromMaybe undefined (Aeson.decode setupBytesJson)
        pure (dbPath, batcherState, setupBytes)
    )
    (\_ → pure ())
    $ \getResources →
      testGroup
        "End-to-end tests"
        [ testCaseSteps "Bridge-in + L2 txs + batch processing" $ \info → withSetup info setup $ \privCtx → do
            (dbPath, batcherState, setupBytes) ← getResources
            -- BSL.writeFile "rollup-aggregator-server/test/data/setup-bytes.json" (Aeson.encode setupBytes)
            -- Step 1: Admin setup — seed rollup and register stake validator
            let fundUser = ctxUserF privCtx
                rollupState0 = stateToRollupState Ex3.prevState
                bridgeOutAddr =
                  "addr_test1qpxsldf6hmp5vtdhhwzukm8x5q0m9t2xh8cftx8s6a43vll3t8hyc5syfx9lltq9dgr2xdkvwahr9humhpa9tae2jcjsxpxw2h"

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
                    , AggCtx.ctxDbPath = dbPath
                    }

            -- Step 4: Bridge-in via handleBridgeIn (10 ADA + 50 asset2)
            let bridgeInValue = valueFromLovelace 10_000_000 <> fakeValue asset2 50_000_000
                birReq =
                  BridgeInRequest
                    { birAmount = bridgeInValue
                    , birDestinationAddress = Ex3.address
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
            SubmitTxResponse {strStatus = status1, strTxHash = txHash1} ← handleSubmitTx aggCtx strReq1
            assertEqual "L2 tx1 queued" "queued" status1
            info "L2 tx1 queued"

            -- Indexing: after tx1, exactly 1 pending tx; getTx returns a pending record with matching hash
            PendingTxsResponse {ptrTxs = ptxs1} ← handlePendingTxs aggCtx
            assertEqual "1 pending tx after tx1" 1 (length ptxs1)
            TxResponse {txrRecord = tx1Rec} ← handleGetTx aggCtx txHash1
            assertEqual "tx1 hash round-trips through getTx" txHash1 (trHash tx1Rec)
            assertEqual "tx1 status is pending" TxPending (trStatus tx1Rec)

            -- tx2: no bridge-outs
            let strReq2 =
                  SubmitTxRequest
                    { strTransaction = Ex3.tx2
                    , strSignatures = perTxSigs !! 1
                    , strBridgeOuts = []
                    }
            SubmitTxResponse {strStatus = status2, strTxHash = txHash2} ← handleSubmitTx aggCtx strReq2
            assertEqual "L2 tx2 queued" "queued" status2
            info "L2 tx2 queued"

            -- Indexing: after tx2, 2 pending txs; no batches recorded yet; no bridge-outs yet
            PendingTxsResponse {ptrTxs = ptxs2} ← handlePendingTxs aggCtx
            assertEqual "2 pending txs after tx2" 2 (length ptxs2)
            BatchesResponse {brsrBatches = batches0} ← handleBatches aggCtx Nothing Nothing
            assertEqual "0 batches before first batch" 0 (length batches0)
            BridgeOutsResponse {borEntries = bouts0} ← handleBridgeOuts aggCtx bridgeOutAddr
            assertEqual "0 bridge-outs before tx3 is submitted" 0 (length bouts0)

            let txCount = natVal (Proxy @Ex3.TxCount)
            queuedTxs ← dequeueTxsDb dbPath txCount
            case queuedTxs of
              Nothing → assertFailure "No transactions in batch queue"
              Just pairs → do
                let (ids, txs) = unzip pairs
                tid ← processBatch aggCtx batcherState ids txs
                info $ "Batch submitted: " <> show tid

            -- Indexing: after batch 1 — no pending txs; 1 batch containing 2 txs; tx1+tx2 are batched;
            -- Ex3.address has transaction history
            PendingTxsResponse {ptrTxs = ptxsAfterB1} ← handlePendingTxs aggCtx
            assertEqual "0 pending txs after batch 1" 0 (length ptxsAfterB1)
            BatchesResponse {brsrBatches = batchesAfterB1} ← handleBatches aggCtx Nothing Nothing
            assertEqual "1 batch recorded after batch 1" 1 (length batchesAfterB1)
            BatchDetailResponse {bdrTxs = b1Txs} ← handleGetBatch aggCtx 1
            assertEqual "batch 1 contains 2 txs" 2 (length b1Txs)
            TxResponse {txrRecord = tx1RecBatched} ← handleGetTx aggCtx txHash1
            assertEqual "tx1 status is batched" TxBatched (trStatus tx1RecBatched)
            TxResponse {txrRecord = tx2RecBatched} ← handleGetTx aggCtx txHash2
            assertEqual "tx2 status is batched" TxBatched (trStatus tx2RecBatched)
            -- tx1 and tx2 each have address as an output address, so both appear in history (newest first)
            TxsByAddressResponse {tarTotal = addrTotal1, tarTxs = addrTxs1} ← handleTxsByAddress aggCtx Ex3.address Nothing Nothing
            assertEqual "Ex3.address has 2 txs after batch 1" 2 addrTotal1
            assertEqual "tx history hashes for Ex3.address (newest first)" [txHash2, txHash1] (map trHash addrTxs1)
            assertBool "all txs in address history are batched" (all ((== TxBatched) . trStatus) addrTxs1)

            -- Step 6b: Query L2 UTxOs after batch 1
            -- After tx1+tx2: address has 5 ADA + 25 asset2, address2 has 5 ADA + 25 asset2
            let [outTx3_1 :*: _, _] = Ex3.tx3 & outputs & unComp1 & fromVector
            QueryL2UtxosResponse utxos1Addr ← handleQueryL2Utxos aggCtx Ex3.address
            assertEqual "UTxO output at address after batch 1" [outTx3_1] (uOutput <$> utxos1Addr)

            QueryL2UtxosResponse utxos1Addr2 ← handleQueryL2Utxos aggCtx Ex3.address2
            assertEqual "UTxO output at address2 after batch 1" [outTx3_1 {oAddress = Ex3.address2}] (uOutput <$> utxos1Addr2)

            -- tx3: 1 bridge-out (5 ADA + 25 asset2 to bridge-out address)
            let bridgeOutValue = valueFromLovelace 5_000_000 <> fakeValue asset2 25_000_000
                strReq3 =
                  SubmitTxRequest
                    { strTransaction = Ex3.tx3
                    , strSignatures = head perTxSigs2
                    , strBridgeOuts = [(bridgeOutValue, bridgeOutAddr)]
                    }
            SubmitTxResponse {strStatus = status3, strTxHash = txHash3} ← handleSubmitTx aggCtx strReq3
            assertEqual "L2 tx3 queued" "queued" status3
            info "L2 tx3 queued"

            -- Indexing: after tx3, 1 pending tx; 1 pending bridge-out for bridgeOutAddr
            PendingTxsResponse {ptrTxs = ptxsAfterTx3} ← handlePendingTxs aggCtx
            assertEqual "1 pending tx after tx3" 1 (length ptxsAfterTx3)
            BridgeOutsResponse {borEntries = boutsAfterTx3} ← handleBridgeOuts aggCtx bridgeOutAddr
            assertEqual "1 bridge-out for bridgeOutAddr after tx3" 1 (length boutsAfterTx3)
            assertEqual "bridge-out is pending" TxPending (boeStatus (head boutsAfterTx3))
            assertEqual "bridge-out tx hash matches tx3" txHash3 (boeTxHash (head boutsAfterTx3))

            -- tx4: no bridge-outs
            let strReq4 =
                  SubmitTxRequest
                    { strTransaction = Ex3.tx4
                    , strSignatures = perTxSigs2 !! 1
                    , strBridgeOuts = []
                    }
            SubmitTxResponse {strStatus = status4, strTxHash = txHash4} ← handleSubmitTx aggCtx strReq4
            assertEqual "L2 tx4 queued" "queued" status4
            info "L2 tx4 queued"

            -- Indexing: after tx4, 2 pending txs (tx3 + tx4)
            PendingTxsResponse {ptrTxs = ptxsAfterTx4} ← handlePendingTxs aggCtx
            assertEqual "2 pending txs (tx3 + tx4)" 2 (length ptxsAfterTx4)

            queuedTxs2 ← dequeueTxsDb dbPath txCount
            case queuedTxs2 of
              Nothing → assertFailure "No transactions in batch queue"
              Just pairs2 → do
                let (ids2, txs2) = unzip pairs2
                tid ← processBatch aggCtx batcherState ids2 txs2
                info $ "Batch submitted: " <> show tid

            -- Indexing: after batch 2 — no pending txs; 2 batches total; batch 2 contains 2 txs;
            -- bridge-out (from tx3) is now batched; Ex3.address has accumulated tx history
            PendingTxsResponse {ptrTxs = ptxsAfterB2} ← handlePendingTxs aggCtx
            assertEqual "0 pending txs after batch 2" 0 (length ptxsAfterB2)
            BatchesResponse {brsrBatches = batchesAfterB2} ← handleBatches aggCtx Nothing Nothing
            assertEqual "2 batches recorded after batch 2" 2 (length batchesAfterB2)
            BatchDetailResponse {bdrTxs = b2Txs} ← handleGetBatch aggCtx 2
            assertEqual "batch 2 contains 2 txs" 2 (length b2Txs)
            BridgeOutsResponse {borEntries = boutsAfterB2} ← handleBridgeOuts aggCtx bridgeOutAddr
            assertEqual "bridge-out still present after batch 2" 1 (length boutsAfterB2)
            assertEqual "bridge-out status is batched" TxBatched (boeStatus (head boutsAfterB2))
            -- tx3 additionally has address as an output AND address as an input (resolved from post-batch-1
            -- preimage since tx2's outputs are there), and tx4 has address as an output — so all 4 txs appear
            TxsByAddressResponse {tarTotal = addrTotal2, tarTxs = addrTxs2} ← handleTxsByAddress aggCtx Ex3.address Nothing Nothing
            assertEqual "Ex3.address has 4 txs after batch 2" 4 addrTotal2
            assertEqual "tx history hashes for Ex3.address (newest first)" [txHash4, txHash3, txHash2, txHash1] (map trHash addrTxs2)
            assertBool "all txs in address history are batched" (all ((== TxBatched) . trStatus) addrTxs2)

            -- Step 8b: Query L2 UTxOs after batch 2
            let [out1 :*: _, out2 :*: _] = Ex3.tx4 & outputs & unComp1 & fromVector
            QueryL2UtxosResponse utxos2Addr ← handleQueryL2Utxos aggCtx Ex3.address
            assertEqual "UTxO output at address after batch 2" [out2] (uOutput <$> utxos2Addr)

            QueryL2UtxosResponse utxos2Addr2 ← handleQueryL2Utxos aggCtx Ex3.address2
            assertEqual "UTxO output at address2 after batch 2" [out1] (uOutput <$> utxos2Addr2)

            info "End-to-end test passed"
        ]
