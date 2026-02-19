module ZkFold.Cardano.Rollup.Aggregator.Batcher (
  BatcherState (..),
  initBatcherState,
  startBatcher,
  enqueueTx,
  processBatch,
  initialState,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (
  TVar,
  atomically,
  newTVarIO,
  readTVar,
  writeTVar,
 )
import Control.Exception (SomeException, try)
import Control.Monad (forM, forever)
import Control.Monad.Reader (asks, runReaderT)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import GHC.Generics ((:*:) (..), (:.:) (..))
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
import ZkFold.Cardano.Rollup.Aggregator.Ctx (Ctx (..), runQuery)
import ZkFold.Cardano.Rollup.Aggregator.Persistence (
  PersistedState (..),
  dequeueTxsDb,
  enqueueTxDb,
  loadState,
  saveState,
 )
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

-- | In-process mutable state and cryptographic material for the batcher.
data BatcherState = BatcherState
  { bsLedgerStateVar ∷ !(TVar (State Bi Bo Ud A I))
  , bsUtxoPreimageVar ∷ !(TVar (Leaves Ud (UTxO A I)))
  , bsTrustedSetup ∷ !(TrustedSetup (LedgerCircuitGates + 6))
  , bsLedgerCircuit ∷ !(LedgerCircuit Bi Bo Ud A Ixs Oxs TxCount)
  , bsProverSecret ∷ !(PlonkupProverSecret BLS12_381_G1_JacobianPoint)
  }

-- | Initialise batcher state by loading persisted state from the SQLite database.
initBatcherState ∷ FilePath → IO BatcherState
initBatcherState dbPath = do
  mPersisted ← loadState dbPath
  let (initSt, initUtxo) = case mPersisted of
        Just (PersistedState st utxo) → (st, utxo)
        Nothing → (initialState, initialUtxoPreimage)
  stateVar ← newTVarIO initSt
  utxoVar ← newTVarIO initUtxo
  ts ← powersOfTauSubset
  let circuit = ledgerCircuit @Bi @Bo @Ud @A @Ixs @Oxs @TxCount @I
      proverSecret = PlonkupProverSecret (pure zero)
  pure $ BatcherState stateVar utxoVar ts circuit proverSecret
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

-- | Enqueue a transaction by writing it to the SQLite database.
enqueueTx ∷ Ctx → QueuedTx → IO ()
enqueueTx Ctx {..} queued = enqueueTxDb ctxDbPath queued

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
-- Takes up to 'Bi' items and pads the rest with 'nullOutput'.
toBridgedIn ∷ [(Integer, GYValue)] → (Vector Bi :.: Output A) I
toBridgedIn items =
  let biCount = fromIntegral (natVal (Proxy @Bi))
      converted = map (uncurry toSymbolicOutput) (take biCount items)
      padded = converted ++ replicate (biCount - length converted) (nullOutput @A @I)
   in Comp1 (unsafeToVector' padded)

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

-- | Run the batcher loop (blocking). Polls the database at the configured interval
-- and processes a batch whenever enough transactions are queued.
startBatcher ∷ Ctx → BatcherState → IO ()
startBatcher ctx@Ctx {..} bs = forever $ do
  let delayMicros = fromIntegral (bcBatchIntervalSeconds ctxBatchConfig) * 1_000_000
  threadDelay delayMicros
  mQueued ← dequeueTxsDb ctxDbPath (bcBatchTransactions ctxBatchConfig)
  case mQueued of
    Nothing → pure ()
    Just queued → do
      result ← try $ processBatch ctx bs queued
      case result of
        Left (err ∷ SomeException) → gyLogError ctxProviders mempty $ "Batch processing failed: " <> show err
        Right tid → gyLogInfo ctxProviders mempty $ "Batch submitted: " <> show tid

processBatch ∷ Ctx → BatcherState → [QueuedTx] → IO GYTxId
processBatch ctx@Ctx {..} BatcherState {..} queuedTxs = do
  (prevState, prevUtxoPreimage) ←
    atomically $
      (,) <$> readTVar bsLedgerStateVar <*> readTVar bsUtxoPreimageVar
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
          bsTrustedSetup
          bsProverSecret
          bsLedgerCircuit
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
        let bridgeInsForL1 = map (\(addr, val) → (val, fromConstant addr)) bridgeInData
        skel ← runReaderT (updateRollupState rollupState bridgeInsForL1 allBridgeOuts proofPlutus) ctxRollupBuildInfo
        body ← buildTxBody skel
        signAndSubmitConfirmed body
  atomically $ do
    writeTVar bsLedgerStateVar newState
    writeTVar bsUtxoPreimageVar newPreimage
  saveState ctxDbPath newState newPreimage
  pure submittedTxId
