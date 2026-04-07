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
  check,
  newTVarIO,
  readTVar,
  readTVarIO,
  writeTVar,
 )
import Control.Exception (Exception, Handler (Handler), catches, displayException, throwIO)
import Control.Monad (forM, forever, when)
import Control.Monad.Reader (asks, runReaderT)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Int (Int64)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word64)
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
  gyLogWarning,
  nonAdaTokenToAssetClass,
  utxoRef,
  utxoValue,
  utxosRemoveTxOutRef,
  utxosToList,
  valueAssetClass,
  valueToPlutus,
 )
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), flattenValue)
import System.Timeout (timeout)
import ZkFold.Algebra.Class (FromConstant (..), zero)
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_JacobianPoint)
import ZkFold.Algebra.EllipticCurve.Class (TwistedEdwards (..))
import ZkFold.Cardano.Rollup.Aggregator.Config (BatchConfig (..))
import ZkFold.Cardano.Rollup.Aggregator.Ctx (Ctx (..), runQuery)
import ZkFold.Cardano.Rollup.Aggregator.Persistence (
  PersistedState (..),
  dequeueAvailableTxsDb,
  dequeueTxsDb,
  enqueueTxDb,
  failTxsDb,
  getPendingTxsWithIdsDb,
  loadState,
  lookupPreimagesByRefDb,
  lookupPreimagesDb,
  recordBatchDb,
  revertProcessingTxsDb,
  revertTxsDb,
  savePreimagesDb,
  seedPreimageDbFromOldState,
 )
import ZkFold.Cardano.Rollup.Aggregator.Types
import ZkFold.Cardano.Rollup.Api (byteStringToInteger', rollupAddress, updateRollupState)
import ZkFold.Cardano.Rollup.Api.Utils (computeDelta, feToInteger, stateToRollupState)
import ZkFold.Cardano.Rollup.Types (ZKInitializedRollupBuildInfo (..))
import ZkFold.Cardano.Rollup.Utils (proofToPlutus)
import ZkFold.Cardano.UPLC.RollupSimple.Types (BridgeUtxoStatus (..))
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Data.Vector (Vector, fromVector)
import ZkFold.Protocol.NonInteractiveProof (TrustedSetup, powersOfTauSubset)
import ZkFold.Protocol.Plonkup.Prover (PlonkupProverSecret (..))
import ZkFold.Symbolic.Data.Bool (BoolType (false))
import ZkFold.Symbolic.Data.Bool qualified as ZkBool
import ZkFold.Symbolic.Data.EllipticCurve.Point.Affine (AffinePoint (..))
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hash (hHash), hash)
import ZkFold.Symbolic.Data.MerkleTree qualified as SymMerkle
import ZkFold.Symbolic.Ledger.Circuit.Compile (
  LedgerCircuit,
  LedgerContractInput (..),
  ledgerCircuit,
  ledgerProof,
  mkProof,
 )
import ZkFold.Symbolic.Ledger.Offchain.State.Update (updateLedgerState)
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Utils (unsafeToVector')

-- | In-process mutable state and cryptographic material for the batcher.
--
-- ChainSync is the single source of truth for state and Merkle tree leaf hashes.
-- The Batcher reads state from ChainSync's TVars and constructs the preimage
-- from the @utxo_preimages@ DB table. The tree is derived from the preimage
-- (guaranteed consistent after the @search@ fix in symbolic-base).
data BatcherState = BatcherState
  { bsLedgerStateVar ∷ !(TVar (State I))
  -- ^ Rollup state. Written by ChainSync only.
  , bsLeafHashesVar ∷ !(TVar (Leaves Ud (FieldElement I)))
  -- ^ Merkle tree leaf hashes. Written by ChainSync only.
  , bsMerkleTreeVar ∷ !(TVar (SymMerkle.MerkleTree Ud I))
  -- ^ Merkle tree derived from leaf hashes. Written by ChainSync only.
  , bsTrustedSetup ∷ !(TrustedSetup (G + 6))
  , bsLedgerCircuit ∷ !(LedgerCircuit Bi Bo Ud A S N TxCount)
  , bsProverSecret ∷ !(PlonkupProverSecret BLS12_381_G1_JacobianPoint)
  , bsStateHistoryVar ∷ !(TVar [(Word64, State I, Leaves Ud (FieldElement I))])
  -- ^ In-memory state history for rollback recovery. Written by ChainSync.
  -- Each entry is (slotNo, state, leafHashes). Most recent first, capped at 20.
  }

-- | Initialise batcher state by loading persisted state from the SQLite database.
initBatcherState ∷ FilePath → IO BatcherState
initBatcherState dbPath = do
  -- One-time migration: seed preimage DB from old-format persisted state.
  seedPreimageDbFromOldState dbPath
  mPersisted ← loadState dbPath
  let (initSt, initLH) = case mPersisted of
        Just (PersistedState st lh) → (st, lh)
        Nothing → (initialState, initialLeafHashes)
      initTree = SymMerkle.fromLeaves initLH
  stateVar ← newTVarIO initSt
  leafHashesVar ← newTVarIO initLH
  treeVar ← newTVarIO initTree
  historyVar ← newTVarIO []
  ts ← powersOfTauSubset
  let circuit = ledgerCircuit @Bi @Bo @Ud @A @S @N @TxCount @I
      proverSecret = PlonkupProverSecret (pure zero)
  pure $ BatcherState stateVar leafHashesVar treeVar ts circuit proverSecret historyVar

initialLeafHashes ∷ Leaves Ud (FieldElement I)
initialLeafHashes = pure (nullUTxOHash @A @I)

emptyTree ∷ SymMerkle.MerkleTree Ud I
emptyTree = SymMerkle.fromLeaves initialLeafHashes

initialState ∷ State I
initialState =
  State
    { sPreviousStateHash = zero
    , sUTxO = SymMerkle.mHash emptyTree
    , sLength = zero
    }

-- | Enqueue a transaction by writing it to the SQLite database.
--
-- If the user provides input UTxOs ('qtInputUtxos'), verifies that:
--   1. Each non-null input ref has a matching user-provided UTxO.
--   2. Each provided UTxO's hash exists in the preimage DB (best-effort).
--
-- If no input UTxOs are provided (empty list), falls back to resolving input
-- addresses from the preimage DB (by output ref lookup).
--
-- Returns the transaction hash (JSON-encoded txId field element).
-- Throws an error if verification fails.
enqueueTx ∷ Ctx → QueuedTx → IO Text
enqueueTx ctx queued = do
  let tx = qtTransaction queued
      providedUtxos = filter (/= nullUTxO) (qtInputUtxos queued)
  if null providedUtxos
    then do
      -- Backward-compatible path: resolve addresses from preimage DB.
      let outs = fromVector (unComp1 (outputs tx))
          outAddrs = map (\(out :*: _) → decodeUtf8 . toStrict . encode $ oAddress out) outs
          inRefs = filter (/= nullOutputRef) $ fromVector (unComp1 (inputs tx))
          refTexts = map (decodeUtf8 . toStrict . encode) inRefs
      matchedUtxos ← lookupPreimagesByRefDb (ctxDbPath ctx) refTexts
      let inAddrs = map (decodeUtf8 . toStrict . encode . oAddress . uOutput) matchedUtxos
      enqueueTxDb (ctxDbPath ctx) queued (nub (outAddrs ++ inAddrs))
    else do
      -- Strict path: user provides input UTxOs.
      let inRefs = filter (/= nullOutputRef) $ fromVector (unComp1 (inputs tx))
      -- Verify: each non-null input ref must have a matching user-provided UTxO.
      let mismatched = [ref | ref ← inRefs, not (any (\u → uRef u == ref) providedUtxos)]
      if not (null mismatched)
        then throwIO $ userError "enqueueTx: input UTxO data missing for one or more non-null inputs"
        else do
          -- Best-effort hash verification against preimage DB.
          let hashTexts =
                [ decodeUtf8 . toStrict . encode $ (hHash (hash u) ∷ FieldElement I)
                | u ← providedUtxos
                ]
          preimageMap ← lookupPreimagesDb (ctxDbPath ctx) hashTexts
          let unknownCount = length [() | ht ← hashTexts, not (Map.member ht preimageMap)]
          when (unknownCount > 0) $
            gyLogWarning (ctxProviders ctx) mempty $
              "enqueueTx: "
                <> show unknownCount
                <> " provided UTxO(s) not found in preimage DB (may be from pending txs or external aggregator)"
          let outs = fromVector (unComp1 (outputs tx))
              outAddrs = map (\(out :*: _) → decodeUtf8 . toStrict . encode $ oAddress out) outs
              inAddrs = map (decodeUtf8 . toStrict . encode . oAddress . uOutput) providedUtxos
          enqueueTxDb (ctxDbPath ctx) queued (nub (outAddrs ++ inAddrs))

-- | Revalidate all pending transactions against the current state.
-- Looks up alive UTxO refs by cross-referencing the current leaf hashes
-- (from ChainSync) with the preimage DB. Transactions referencing unknown
-- or consumed inputs are marked as 'failed'.
revalidatePendingTxs ∷ Ctx → BatcherState → IO ()
revalidatePendingTxs Ctx {..} BatcherState {..} = do
  pending ← getPendingTxsWithIdsDb ctxDbPath
  when (not (null pending)) $ do
    leafHashes ← readTVarIO bsLeafHashesVar
    let nullHash = nullUTxOHash @A @I
        nonNullHashTexts =
          [ decodeUtf8 . toStrict . encode $ h
          | h ← fromVector leafHashes
          , h /= nullHash
          ]
    preimageMap ← lookupPreimagesDb ctxDbPath nonNullHashTexts
    let knownRefs = map uRef $ Map.elems preimageMap
        isInputValid ref = ref == nullOutputRef || ref `elem` knownRefs
        invalidIds =
          [ tid
          | (tid, qtx) ← pending
          , let inRefs = fromVector (unComp1 (inputs (qtTransaction qtx)))
          , not (all isInputValid inRefs)
          ]
    when (not (null invalidIds)) $ do
      gyLogInfo ctxProviders mempty $
        "Revalidation: failing "
          <> show (length invalidIds)
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
startBatcher ctx@Ctx {..} bs = do
  -- Track ChainSync's chain length to detect external updates and for waitForChainSync.
  lastChainSyncLenRef ← readTVarIO (bsLedgerStateVar bs) >>= newTVarIO . feToInteger . sLength
  forever $ do
    let delayMicros = fromIntegral (bcBatchIntervalSeconds ctxBatchConfig) * 1_000_000
    threadDelay delayMicros
    -- Detect state changes from ChainSync (external updates or rollbacks).
    -- bsLedgerStateVar is written ONLY by ChainSync, so changes indicate
    -- on-chain state updates.
    currentLen ← feToInteger . sLength <$> readTVarIO (bsLedgerStateVar bs)
    prevLen ← readTVarIO lastChainSyncLenRef
    when (currentLen /= prevLen) $ do
      gyLogInfo ctxProviders mempty $
        "ChainSync state changed (len "
          <> show prevLen
          <> " → "
          <> show currentLen
          <> "), revalidating pending txs"
      revertProcessingTxsDb ctxDbPath
      revalidatePendingTxs ctx bs
      atomically $ writeTVar lastChainSyncLenRef currentLen
    -- Process batches.
    bridgeInData ← queryBridgeIns ctx
    if not (null bridgeInData)
      then do
        -- Bridge-ins pending: trigger a batch even with fewer than TxCount real txs,
        -- padding the remainder with null transactions.
        let txCount = fromIntegral (natVal (Proxy @TxCount))
        available ← dequeueAvailableTxsDb ctxDbPath (bcBatchTransactions ctxBatchConfig)
        let (ids, qtxs) = unzip available
            padded = qtxs ++ replicate (txCount - length qtxs) nullQueuedTx
        processBatchWithLogging ctx bs ids padded lastChainSyncLenRef
      else do
        mQueued ← dequeueTxsDb ctxDbPath (bcBatchTransactions ctxBatchConfig)
        for_ mQueued $ \pairs →
          let (ids, qtxs) = unzip pairs
           in processBatchWithLogging ctx bs ids qtxs lastChainSyncLenRef

processBatchWithLogging ∷ Ctx → BatcherState → [Int64] → [QueuedTx] → TVar Integer → IO ()
processBatchWithLogging ctx@Ctx {..} bs ids queued lastLenRef =
  ( do
      tid ← processBatch ctx bs ids queued
      gyLogInfo ctxProviders mempty $ "Batch submitted: " <> show tid
      -- Wait for ChainSync to process the block.
      waitForChainSync ctx bs lastLenRef
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

-- | Wait for ChainSync to process the block by watching ChainSync's own
-- state TVar (which only ChainSync writes to). Times out after 120 seconds.
waitForChainSync ∷ Ctx → BatcherState → TVar Integer → IO ()
waitForChainSync Ctx {..} BatcherState {..} lastChainSyncLenRef = do
  prevLen ← readTVarIO lastChainSyncLenRef
  gyLogInfo ctxProviders mempty "Waiting for ChainSync to confirm state update..."
  mResult ← timeout 120_000_000 $ atomically $ do
    st ← readTVar bsLedgerStateVar
    let currentLen = feToInteger (sLength st)
    check (currentLen > prevLen)
  case mResult of
    Nothing →
      gyLogWarning
        ctxProviders
        mempty
        "Timed out waiting for ChainSync (120s). Proceeding."
    Just () → do
      newLen ← feToInteger . sLength <$> readTVarIO bsLedgerStateVar
      atomically $ writeTVar lastChainSyncLenRef newLen
      gyLogInfo ctxProviders mempty "ChainSync confirmed state update"

-- | Construct the UTxO preimage vector by looking up leaf hashes in the preimage DB.
-- For each leaf hash:
--   * nullUTxOHash → nullUTxO (empty slot, no DB lookup needed)
--   * found in DB → the stored UTxO
--   * not found in DB → nullUTxO (unknown external UTxO)
constructPreimage ∷ FilePath → Leaves Ud (FieldElement I) → IO (Leaves Ud (UTxO A I))
constructPreimage dbPath leafHashes = do
  let nullHash = nullUTxOHash @A @I
      hashList = fromVector leafHashes
      nonNullHashes = filter (/= nullHash) hashList
      nonNullHashTexts = map (decodeUtf8 . toStrict . encode) nonNullHashes
  preimageMap ← lookupPreimagesDb dbPath nonNullHashTexts
  let missed = length nonNullHashes - Map.size preimageMap
  when (missed > 0) $
    -- This indicates a key mismatch between what ChainSync stored
    -- (from delta) and what the Batcher stored (from updateLedgerState).
    putStrLn $
      "constructPreimage: "
        <> show missed
        <> " of "
        <> show (length nonNullHashes)
        <> " non-null leaf hashes NOT found in preimage DB"
  pure $
    fmap
      ( \h →
          if h == nullHash
            then nullUTxO @A @I
            else
              let key = decodeUtf8 . toStrict . encode $ h
               in Map.findWithDefault (nullUTxO @A @I) key preimageMap
      )
      leafHashes

processBatch ∷ Ctx → BatcherState → [Int64] → [QueuedTx] → IO GYTxId
processBatch ctx@Ctx {..} BatcherState {..} ids queuedTxs = do
  -- Read state from ChainSync's TVar, construct preimage from DB, derive tree.
  -- With the search fix in symbolic-base, fromLeaves(fmap(hHash.hash) preimage)
  -- produces the same tree as updateLedgerState's output.
  (prevState, prevLeafHashes) ←
    atomically $
      (,) <$> readTVar bsLedgerStateVar <*> readTVar bsLeafHashesVar
  prevUtxoPreimage ← constructPreimage ctxDbPath prevLeafHashes
  let prevTree = SymMerkle.fromLeaves (fmap (hHash . hash) prevUtxoPreimage)
  bridgeInData ← queryBridgeIns ctx
  let bridgedIn = toBridgedIn bridgeInData
      batch = TransactionBatch {tbTransactions = unsafeToVector' (map qtTransaction queuedTxs)}
      sigMaterial = Comp1 (unsafeToVector' (map qtSignatures queuedTxs))
      allBridgeOuts = concatMap qtBridgeOuts queuedTxs
      newState :*: witness :*: _newTree :*: preimageWrapped =
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
        ledgerProof @_ @ByteString
          bsTrustedSetup
          bsProverSecret
          bsLedgerCircuit
          lci
      proofBytes = mkProof proof
      proofPlutus = proofToPlutus proofBytes
      rollupState = stateToRollupState newState
      delta = computeDelta witness batch bridgedIn newState
      collateral = Just (ctxCollateral, False)
  -- Store new UTxO preimages in the DB (for ChainSync / next batch to look up).
  let newEntries =
        [ (hHash (hash utxo), uRef utxo, utxo)
        | utxo ← fromVector newPreimage
        , utxo /= nullUTxO @A @I
        ]
  savePreimagesDb ctxDbPath newEntries
  -- Submit the L1 transaction.
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
  -- ChainSync will update state/leafHashes/tree TVars when it sees the block.
  recordBatchDb ctxDbPath ids (Text.pack (show submittedTxId))
  pure submittedTxId
