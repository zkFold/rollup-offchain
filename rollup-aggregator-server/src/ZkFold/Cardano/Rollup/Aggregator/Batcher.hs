module ZkFold.Cardano.Rollup.Aggregator.Batcher (
  BatcherState (..),
  initBatcherState,
  startBatcher,
  enqueueTx,
  processBatch,
  initialState,
  queryBridgeIns,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (
  TVar,
  atomically,
  newTVarIO,
  readTVar,
  readTVarIO,
  writeTVar,
 )
import Control.Exception (Exception, Handler (Handler), catches, displayException, throwIO)
import Control.Monad (forM, forever)
import Data.List (nub)
import Control.Monad.Reader (asks, runReaderT)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics ((:*:) (..), (:.:) (..))
import GHC.TypeNats (natVal, type (+))
import GeniusYield.Providers.Blockfrost (BlockfrostProviderException)
import GeniusYield.Providers.Common (SubmitTxException)
import GeniusYield.Providers.Kupo (KupoProviderException)
import GeniusYield.Providers.Maestro (MaestroProviderException)
import GeniusYield.Providers.Ogmios (OgmiosProviderException)
import GeniusYield.TxBuilder (buildTxBody, runGYTxMonadIO, signAndSubmitConfirmed, utxoDatum, utxosAtAddress)
import GeniusYield.TxBuilder.Errors (GYTxMonadException)
import GeniusYield.Types (
  GYAwaitTxException,
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
import ZkFold.Algebra.EllipticCurve.Class (TwistedEdwards (..))
import ZkFold.Symbolic.Data.EllipticCurve.Point.Affine (AffinePoint (..))
import ZkFold.Cardano.Rollup.Aggregator.Config (BatchConfig (..))
import ZkFold.Cardano.Rollup.Aggregator.Ctx (Ctx (..), runQuery)
import ZkFold.Cardano.Rollup.Aggregator.Persistence (
  PersistedState (..),
  dequeueAvailableTxsDb,
  dequeueTxsDb,
  enqueueTxDb,
  loadState,
  recordBatchDb,
  failTxsDb,
  getPendingTxsWithIdsDb,
  revertProcessingTxsDb,
  revertTxsDb,
  saveState,
 )
import ZkFold.Cardano.Rollup.Aggregator.Types
import ZkFold.Cardano.Rollup.Api (byteStringToInteger', rollupAddress, updateRollupState)
import ZkFold.Cardano.Rollup.Api.Utils (computeDelta, stateToRollupState)
import ZkFold.Cardano.Rollup.Types (ZKInitializedRollupBuildInfo (..))
import ZkFold.Cardano.Rollup.Utils (proofToPlutus)
import ZkFold.Cardano.UPLC.RollupSimple.Types (BridgeUtxoStatus (..))
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Data.Vector (Vector, fromVector)
import ZkFold.Protocol.NonInteractiveProof (TrustedSetup, powersOfTauSubset)
import ZkFold.Protocol.Plonkup.Prover (PlonkupProverSecret (..))
import ZkFold.Symbolic.Data.Bool (BoolType (false, true))
import ZkFold.Symbolic.Data.Bool qualified as ZkBool
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hash (hHash), hash)
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
  { bsLedgerStateVar ∷ !(TVar (State I))
  , bsUtxoPreimageVar ∷ !(TVar (Leaves Ud (UTxO A I)))
  , bsMerkleTreeVar ∷ !(TVar (SymMerkle.MerkleTree Ud I))
  , bsTrustedSetup ∷ !(TrustedSetup (LedgerCircuitGates + 6))
  , bsLedgerCircuit ∷ !(LedgerCircuit Bi Bo Ud A S N TxCount)
  , bsProverSecret ∷ !(PlonkupProverSecret BLS12_381_G1_JacobianPoint)
  , bsExternalUpdateVar ∷ !(TVar Bool)
  -- ^ Set to True by chain sync when state was updated by another aggregator.
  }

-- | Initialise batcher state by loading persisted state from the SQLite database.
initBatcherState ∷ FilePath → IO BatcherState
initBatcherState dbPath = do
  mPersisted ← loadState dbPath
  let (initSt, initUtxo) = case mPersisted of
        Just (PersistedState st utxo) → (st, utxo)
        Nothing → (initialState, initialUtxoPreimage)
      initTree = SymMerkle.fromLeaves (fmap (hHash . hash) initUtxo)
  stateVar ← newTVarIO initSt
  utxoVar ← newTVarIO initUtxo
  treeVar ← newTVarIO initTree
  externalUpdateVar ← newTVarIO False
  ts ← powersOfTauSubset
  let circuit = ledgerCircuit @Bi @Bo @Ud @A @S @N @TxCount @I
      proverSecret = PlonkupProverSecret (pure zero)
  pure $ BatcherState stateVar utxoVar treeVar ts circuit proverSecret externalUpdateVar
 where
  initialUtxoPreimage = pure (nullUTxO @A @I)

emptyTree ∷ SymMerkle.MerkleTree Ud I
emptyTree = SymMerkle.fromLeaves (pure (nullUTxOHash @A @I))

initialState ∷ State I
initialState =
  State
    { sPreviousStateHash = zero
    , sUTxO = SymMerkle.mHash emptyTree
    , sLength = zero
    }

-- | Enqueue a transaction by writing it to the SQLite database.
-- Verifies that:
--   1. Each non-null input ref has a matching user-provided UTxO.
--   2. Each provided UTxO's hash exists in the persisted Merkle tree (best-effort check).
-- Returns the transaction hash (JSON-encoded txId field element).
-- Throws an error if verification fails.
enqueueTx ∷ Ctx → QueuedTx → IO Text
enqueueTx ctx queued = do
  let tx = qtTransaction queued
      inRefs = filter (/= nullOutputRef) $ fromVector (unComp1 (inputs tx))
      providedUtxos = filter (/= nullUTxO) (qtInputUtxos queued)
  -- Verify: each non-null input ref must have a matching user-provided UTxO.
  let mismatched = [ ref | ref ← inRefs, not (any (\u → uRef u == ref) providedUtxos) ]
  if not (null mismatched)
    then throwIO $ userError "enqueueTx: input UTxO data missing for one or more non-null inputs"
    else do
      -- Best-effort hash verification against persisted preimage.
      mState ← loadState (ctxDbPath ctx)
      case mState of
        Just ps → do
          let preimageHashes ∷ [FieldElement I] =
                map (hHash . hash) $ fromVector (psUtxoPreimage ps)
          let invalidUtxos =
                [ u
                | u ← providedUtxos
                , let h ∷ FieldElement I = hHash (hash u)
                , h `notElem` preimageHashes
                ]
          if not (null invalidUtxos)
            then throwIO $ userError "enqueueTx: provided UTxO hash does not match any leaf in the Merkle tree"
            else pure ()
        Nothing → pure () -- No persisted state yet, skip verification.
      let outs = fromVector (unComp1 (outputs tx))
          outAddrs = map (\(out :*: _) → decodeUtf8 . toStrict . encode $ oAddress out) outs
          inAddrs = map (decodeUtf8 . toStrict . encode . oAddress . uOutput) providedUtxos
      enqueueTxDb (ctxDbPath ctx) queued (nub (outAddrs ++ inAddrs))

-- | Revalidate all pending transactions against the current in-memory state.
-- Transactions whose non-null input OutputRefs don't match any known UTxO in the
-- preimage are marked as 'failed' (their inputs were consumed by another aggregator).
revalidatePendingTxs ∷ Ctx → BatcherState → IO ()
revalidatePendingTxs Ctx {..} BatcherState {..} = do
  pending ← getPendingTxsWithIdsDb ctxDbPath
  preimage ← readTVarIO bsUtxoPreimageVar
  let knownRefs = map uRef $ filter (/= nullUTxO) $ fromVector preimage
      isInputValid ref = ref == nullOutputRef || ref `elem` knownRefs
      invalidIds =
        [ tid
        | (tid, qtx) ← pending
        , let inRefs = fromVector (unComp1 (inputs (qtTransaction qtx)))
        , not (all isInputValid inRefs)
        ]
  if null invalidIds
    then pure ()
    else do
      gyLogInfo ctxProviders mempty $
        "Revalidation: failing " <> show (length invalidIds)
          <> " pending txs with consumed inputs"
      failTxsDb ctxDbPath invalidIds

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

-- | A null transaction: all inputs are 'nullOutputRef', all outputs are 'nullOutput'.
-- The circuit skips signature verification and Merkle tree operations for null
-- inputs/outputs, so this can safely pad a batch when fewer real transactions
-- are available than the circuit's fixed 'TxCount'.
nullQueuedTx ∷ QueuedTx
nullQueuedTx =
  QueuedTx
    { qtTransaction =
        Transaction
          { inputs = Comp1 (pure nullOutputRef)
          , outputs = Comp1 (pure (nullOutput @A @I :*: (false ∷ ZkBool.Bool I)))
          }
    , qtSignatures = Comp1 (pure (zero :*: zero :*: zero))
    , qtBridgeOuts = []
    , qtInputUtxos = []
    }

-- | Run the batcher loop (blocking). Polls the database at the configured interval
-- and processes a batch when either:
-- * enough real transactions are queued (≥ bcBatchTransactions), or
-- * there are pending bridge-ins on L1 (remaining slots are padded with null txs).
startBatcher ∷ Ctx → BatcherState → IO ()
startBatcher ctx@Ctx {..} bs = forever $ do
  let delayMicros = fromIntegral (bcBatchIntervalSeconds ctxBatchConfig) * 1_000_000
  threadDelay delayMicros
  -- Check for external state updates from chain sync.
  externalUpdate ← atomically $ do
    updated ← readTVar (bsExternalUpdateVar bs)
    writeTVar (bsExternalUpdateVar bs) False
    pure updated
  if externalUpdate
    then do
      gyLogInfo ctxProviders mempty "External state update detected, re-syncing"
      -- Revert any 'processing' txs back to 'pending' (they may reference stale state).
      revertProcessingTxsDb ctxDbPath
      -- Revalidate pending txs: fail those whose inputs no longer exist.
      revalidatePendingTxs ctx bs
    else do
      pure ()
  bridgeInData ← queryBridgeIns ctx
  if not (null bridgeInData)
    then do
      -- Bridge-ins pending: trigger a batch even with fewer than TxCount real txs,
      -- padding the remainder with null transactions.
      let txCount = fromIntegral (natVal (Proxy @TxCount))
      available ← dequeueAvailableTxsDb ctxDbPath (bcBatchTransactions ctxBatchConfig)
      let (ids, qtxs) = unzip available
          padded = qtxs ++ replicate (txCount - length qtxs) nullQueuedTx
      processBatchWithLogging ctx bs ids padded
    else do
      mQueued ← dequeueTxsDb ctxDbPath (bcBatchTransactions ctxBatchConfig)
      for_ mQueued $ \pairs →
        let (ids, qtxs) = unzip pairs
         in processBatchWithLogging ctx bs ids qtxs

processBatchWithLogging ∷ Ctx → BatcherState → [Int64] → [QueuedTx] → IO ()
processBatchWithLogging ctx@Ctx {..} bs ids queued =
  ( do
      tid ← processBatch ctx bs ids queued
      gyLogInfo ctxProviders mempty $ "Batch submitted: " <> show tid
  )
    `catches` [ Handler (\(err ∷ GYTxMonadException) → revert >> logException "GYTxMonadException" err)
              , Handler (\(err ∷ SubmitTxException) → revert >> logException "SubmitTxException" err)
              , Handler (\(err ∷ GYAwaitTxException) → revert >> logException "GYAwaitTxException" err)
              , Handler (\(err ∷ BlockfrostProviderException) → revert >> logException "BlockfrostProviderException" err)
              , Handler (\(err ∷ MaestroProviderException) → revert >> logException "MaestroProviderException" err)
              , Handler (\(err ∷ KupoProviderException) → revert >> logException "KupoProviderException" err)
              , Handler (\(err ∷ OgmiosProviderException) → revert >> logException "OgmiosProviderException" err)
              ]
 where
  revert = revertTxsDb ctxDbPath ids
  logException ∷ Exception e ⇒ String → e → IO ()
  logException label err =
    gyLogError ctxProviders mempty $
      "Batch processing failed (" <> label <> "): " <> displayException err

processBatch ∷ Ctx → BatcherState → [Int64] → [QueuedTx] → IO GYTxId
processBatch ctx@Ctx {..} BatcherState {..} ids queuedTxs = do
  (prevState, prevUtxoPreimage, prevTree) ←
    atomically $
      (,,) <$> readTVar bsLedgerStateVar <*> readTVar bsUtxoPreimageVar <*> readTVar bsMerkleTreeVar
  bridgeInData ← queryBridgeIns ctx
  let bridgedIn = toBridgedIn bridgeInData
      batch = TransactionBatch {tbTransactions = unsafeToVector' (map qtTransaction queuedTxs)}
      sigMaterial = Comp1 (unsafeToVector' (map qtSignatures queuedTxs))
      allBridgeOuts = concatMap qtBridgeOuts queuedTxs
      newState :*: witness :*: newTree :*: preimageWrapped =
        updateLedgerState prevState prevTree prevUtxoPreimage bridgedIn batch sigMaterial
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
      delta = computeDelta witness batch bridgedIn newState
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
        -- Only pass as many bridge-ins as the circuit handles (Bi items), matching
        -- the prefix taken by toBridgedIn. Passing more would cause the L1 validator
        -- to see BridgeIn outputs not covered by the ZK proof.
        let biCount = fromIntegral (natVal (Proxy @Bi))
            bridgeInsForL1 = take biCount $ map (\(addr, val) → (val, fromConstant addr)) bridgeInData
        skel ← runReaderT (updateRollupState rollupState bridgeInsForL1 allBridgeOuts proofPlutus delta) ctxRollupBuildInfo
        body ← buildTxBody skel
        signAndSubmitConfirmed body
  atomically $ do
    writeTVar bsLedgerStateVar newState
    writeTVar bsUtxoPreimageVar newPreimage
    writeTVar bsMerkleTreeVar newTree
  saveState ctxDbPath newState newPreimage
  recordBatchDb ctxDbPath ids (Text.pack (show submittedTxId))
  pure submittedTxId
