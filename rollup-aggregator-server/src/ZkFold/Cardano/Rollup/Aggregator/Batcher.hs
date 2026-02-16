module ZkFold.Cardano.Rollup.Aggregator.Batcher (
  initBatcherState,
  startBatcher,
  enqueueTx,
  processBatch,
  drainQueue,
  initialState,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (
  STM,
  TQueue,
  TVar,
  atomically,
  newTQueueIO,
  newTVarIO,
  readTVar,
  tryReadTQueue,
  writeTQueue,
  writeTVar,
 )
import Control.Exception (SomeException, try)
import Control.Monad (forM, forever, void)
import Control.Monad.Reader (asks, runReaderT)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import GHC.Generics ((:*:) (..), (:.:) (..))
import GHC.IsList (fromList)
import GHC.TypeNats (natVal, type (+))
import GeniusYield.TxBuilder (buildTxBody, runGYTxMonadIO, signAndSubmitConfirmed, utxoDatum, utxosAtAddress)
import GeniusYield.Types (
  GYTxId,
  GYValue,
  filterUTxOs,
  gyLogError,
  gyLogInfo,
  nonAdaTokenToAssetClass,
  utxoRef,
  utxoValue,
  utxosRemoveTxOutRef,
  utxosToList,
  valueAssetClass,
  valueToPlutus,
 )
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), flattenValue)
import ZkFold.Algebra.Class (FromConstant (..), zero)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_JacobianPoint)
import ZkFold.Cardano.Rollup.Aggregator.Config (BatchConfig (..))
import ZkFold.Cardano.Rollup.Aggregator.Persistence (PersistedState (..), saveState)
import ZkFold.Cardano.Rollup.Aggregator.Ctx (Ctx (..), runQuery)
import ZkFold.Cardano.Rollup.Aggregator.Types
import ZkFold.Cardano.Rollup.Api (byteStringToInteger', rollupAddress, updateRollupState)
import ZkFold.Cardano.Rollup.Api.Utils (stateToRollupState)
import ZkFold.Cardano.Rollup.Types (ZKInitializedRollupBuildInfo (..))
import ZkFold.Cardano.Rollup.Utils (proofToPlutus)
import ZkFold.Cardano.UPLC.RollupSimple.Types (BridgeUtxoStatus (..))
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Data.Vector (Vector)
import ZkFold.Protocol.NonInteractiveProof (TrustedSetup, powersOfTauSubset)
import ZkFold.Protocol.Plonkup.Prover (PlonkupProverSecret (..))
import ZkFold.Symbolic.Data.Hash (hash)
import ZkFold.Symbolic.Data.MerkleTree qualified as SymMerkle
import ZkFold.Symbolic.Ledger.Circuit.Compile (
  LedgerCircuit,
  LedgerCircuitGates,
  LedgerContractInput (..),
  ledgerCircuit,
  ledgerProof,
  mkProof,
 )
import ZkFold.Symbolic.Ledger.Offchain.State.Update (updateLedgerState)
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Utils (unsafeToVector')

initBatcherState
  ∷ Maybe PersistedState
  → IO
      ( TQueue QueuedTx
      , TVar (State Bi Bo Ud A I)
      , TVar (Leaves Ud (UTxO A I))
      , TrustedSetup (LedgerCircuitGates + 6)
      , LedgerCircuit Bi Bo Ud A Ixs Oxs TxCount
      , PlonkupProverSecret BLS12_381_G1_JacobianPoint
      )
initBatcherState mPersisted = do
  queue ← newTQueueIO
  let (initSt, initUtxo) = case mPersisted of
        Just (PersistedState st utxo) → (st, utxo)
        Nothing → (initialState, initialUtxoPreimage)
  stateVar ← newTVarIO initSt
  utxoVar ← newTVarIO initUtxo
  ts ← powersOfTauSubset
  let circuit = ledgerCircuit @Bi @Bo @Ud @A @Ixs @Oxs @TxCount @I
      proverSecret = PlonkupProverSecret (pure zero)
  pure (queue, stateVar, utxoVar, ts, circuit, proverSecret)
 where
  initialUtxoPreimage = pure (nullUTxO @A @I)

emptyTree ∷ SymMerkle.MerkleTree Ud I
emptyTree = SymMerkle.fromLeaves (pure (nullUTxOHash @A @I))

initialState ∷ State Bi Bo Ud A I
initialState =
  State
    { sPreviousStateHash = zero
    , sUTxO = emptyTree
    , sLength = zero
    , sBridgeIn = hash (Comp1 (pure (nullOutput @A @I)))
    , sBridgeOut = hash (Comp1 (pure (nullOutput @A @I)))
    }

enqueueTx ∷ Ctx → QueuedTx → IO ()
enqueueTx Ctx {..} queued = atomically $ writeTQueue ctxBatchQueue queued

-- | Query the rollup address for 'BridgeInInitial' UTxOs, returning L2 address and value.
queryBridgeIns ∷ Ctx → IO [(Integer, GYValue)]
queryBridgeIns ctx = runQuery ctx $ do
  nft ← asks zkirbiNFT
  rollupAddr ← rollupAddress
  allUtxos ← utxosAtAddress rollupAddr Nothing
  let stateUtxos =
        filterUTxOs (\u → valueAssetClass (utxoValue u) (nonAdaTokenToAssetClass nft) == 1) allUtxos
          & utxosToList
  case stateUtxos of
    [stateUtxo] → do
      let others = utxosRemoveTxOutRef (utxoRef stateUtxo) allUtxos & utxosToList
      initials ← forM others $ \u → do
        datumTuple ← utxoDatum @_ @BridgeUtxoStatus u
        case datumTuple of
          Right (_, _, BridgeInInitial addr) → pure $ Just (addr, utxoValue u)
          _ → pure Nothing
      pure $ catMaybes initials
    _ → pure []

-- | Convert a list of (L2 address, GYValue) pairs into the symbolic bridge-in representation.
toBridgedIn ∷ [(Integer, GYValue)] → (Vector Bi :.: Output A) I
toBridgedIn [] = Comp1 (fromList [nullOutput @A @I])
toBridgedIn ((addr, val) : _) = Comp1 (fromList [toSymbolicOutput addr val])

-- | Convert an L2 address and a 'GYValue' into a symbolic 'Output'.
toSymbolicOutput ∷ Integer → GYValue → Output A I
toSymbolicOutput addr val =
  Output
    { oAddress = fromConstant addr
    , oAssets =
        Comp1 $
          unsafeToVector' $
            map toAsset flatAssets
              ++ replicate (aCount - length flatAssets) (nullAssetValue @I)
    }
 where
  flatAssets = reverse $ flattenValue (valueToPlutus val)
  aCount = fromIntegral (natVal (Proxy @A))
  toAsset (cs, tn, amt) =
    AssetValue
      { assetPolicy = fromConstant (byteStringToInteger' (unCurrencySymbol cs))
      , assetName = fromConstant (byteStringToInteger' (unTokenName tn))
      , assetQuantity = fromConstant amt
      }

-- | Drain all available items from a 'TQueue'.
drainQueue ∷ TQueue a → STM [a]
drainQueue q = do
  mx ← tryReadTQueue q
  case mx of
    Nothing → pure []
    Just x → (x :) <$> drainQueue q

startBatcher ∷ Ctx → IO ()
startBatcher ctx@Ctx {..} = void . async . forever $ do
  let delayMicros = fromIntegral (bcBatchIntervalSeconds ctxBatchConfig) * 1_000_000
  threadDelay delayMicros
  queued ← atomically $ drainQueue ctxBatchQueue
  case queued of
    [] → pure ()
    _ → do
      result ← try $ processBatch ctx queued
      case result of
        Left (err ∷ SomeException) → gyLogError ctxProviders mempty $ "Batch processing failed: " <> show err
        Right tid → gyLogInfo ctxProviders mempty $ "Batch submitted: " <> show tid

processBatch ∷ Ctx → [QueuedTx] → IO GYTxId
processBatch ctx@Ctx {..} queuedTxs = do
  (prevState, prevUtxoPreimage) ←
    atomically $
      (,) <$> readTVar ctxLedgerStateVar <*> readTVar ctxUtxoPreimageVar
  bridgeInData ← queryBridgeIns ctx
  let bridgedIn = toBridgedIn bridgeInData
      batch = TransactionBatch {tbTransactions = unsafeToVector' (map qtTransaction queuedTxs)}
      sigMaterial = Comp1 (unsafeToVector' (map qtSignatures queuedTxs))
      allBridgeOuts = concatMap qtBridgeOuts queuedTxs
      newState :*: witness :*: preimageWrapped =
        updateLedgerState prevState prevUtxoPreimage bridgedIn batch sigMaterial
      newPreimage = unComp1 preimageWrapped
      lci =
        LedgerContractInput
          { lciPreviousState = prevState
          , lciTransactionBatch = batch
          , lciNewState = newState
          , lciStateWitness = witness
          }
      proof =
        ledgerProof @ByteString
          ctxTrustedSetup
          ctxProverSecret
          ctxLedgerCircuit
          lci
      proofBytes = mkProof proof
      proofPlutus = proofToPlutus proofBytes
      rollupState = stateToRollupState newState
      collateral = Just (ctxCollateral, False)
  submittedTxId ←
    runGYTxMonadIO
      ctxNetworkId
      ctxProviders
      (fst ctxSigningKey)
      Nothing
      [snd ctxSigningKey]
      (snd ctxSigningKey)
      collateral
      $ do
        skel ← runReaderT (updateRollupState rollupState [] allBridgeOuts proofPlutus) ctxRollupBuildInfo
        body ← buildTxBody skel
        signAndSubmitConfirmed body
  atomically $ do
    writeTVar ctxLedgerStateVar newState
    writeTVar ctxUtxoPreimageVar newPreimage
  case ctxStatePersistPath of
    Just persistPath → saveState persistPath newState newPreimage
    Nothing → pure ()
  pure submittedTxId
