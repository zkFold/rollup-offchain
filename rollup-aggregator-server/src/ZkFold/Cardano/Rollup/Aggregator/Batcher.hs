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
  writeTVar,
 )
import Control.Exception (Exception, Handler (Handler), catches, displayException)
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
import ZkFold.Cardano.Rollup.Aggregator.Config (BatchConfig (..))
import ZkFold.Cardano.Rollup.Aggregator.Ctx (Ctx (..), runQuery)
import ZkFold.Cardano.Rollup.Aggregator.Persistence (
  PersistedState (..),
  dequeueAvailableTxsDb,
  dequeueTxsDb,
  enqueueTxDb,
  loadState,
  recordBatchDb,
  revertTxsDb,
  saveState,
 )
import ZkFold.Cardano.Rollup.Aggregator.Types
import ZkFold.Cardano.Rollup.Api (byteStringToInteger', rollupAddress, updateRollupState)
import ZkFold.Cardano.Rollup.Api.Utils (stateToRollupState)
import ZkFold.Cardano.Rollup.Types (ZKInitializedRollupBuildInfo (..))
import ZkFold.Cardano.Rollup.Utils (proofToPlutus)
import ZkFold.Cardano.UPLC.RollupSimple.Types (BridgeUtxoStatus (..))
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Data.Vector (Vector, fromVector)
import ZkFold.Protocol.NonInteractiveProof (TrustedSetup, powersOfTauSubset)
import ZkFold.Protocol.Plonkup.Prover (PlonkupProverSecret (..))
import ZkFold.Symbolic.Data.Bool (BoolType (false))
import ZkFold.Symbolic.Data.Bool qualified as ZkBool
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
-- Both output addresses (receiving) and input addresses (spending, resolved from
-- the persisted UTxO preimage) are stored in 'tx_addresses' for indexed lookup.
-- Returns the transaction hash (JSON-encoded txId field element).
enqueueTx ∷ Ctx → QueuedTx → IO Text
enqueueTx ctx queued = do
  let tx = qtTransaction queued
      outs = fromVector (unComp1 (outputs tx))
      outAddrs = map (\(out :*: _) → decodeUtf8 . toStrict . encode $ oAddress out) outs
      inRefs = filter (/= nullOutputRef) $ fromVector (unComp1 (inputs tx))
  mState ← loadState (ctxDbPath ctx)
  let inAddrs = case mState of
        Nothing → []
        Just ps →
          let utxoList = filter (/= nullUTxO) $ fromVector (psUtxoPreimage ps)
           in [ decodeUtf8 . toStrict . encode $ oAddress (uOutput u)
              | ref ← inRefs, u ← utxoList, uRef u == ref
              ]
  enqueueTxDb (ctxDbPath ctx) queued (nub (outAddrs ++ inAddrs))

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
    }

-- | Run the batcher loop (blocking). Polls the database at the configured interval
-- and processes a batch when either:
-- * enough real transactions are queued (≥ bcBatchTransactions), or
-- * there are pending bridge-ins on L1 (remaining slots are padded with null txs).
startBatcher ∷ Ctx → BatcherState → IO ()
startBatcher ctx@Ctx {..} bs = forever $ do
  let delayMicros = fromIntegral (bcBatchIntervalSeconds ctxBatchConfig) * 1_000_000
  threadDelay delayMicros
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
        -- Only pass as many bridge-ins as the circuit handles (Bi items), matching
        -- the prefix taken by toBridgedIn. Passing more would cause the L1 validator
        -- to see BridgeIn outputs not covered by the ZK proof.
        let biCount = fromIntegral (natVal (Proxy @Bi))
            bridgeInsForL1 = take biCount $ map (\(addr, val) → (val, fromConstant addr)) bridgeInData
        skel ← runReaderT (updateRollupState rollupState bridgeInsForL1 allBridgeOuts proofPlutus) ctxRollupBuildInfo
        body ← buildTxBody skel
        signAndSubmitConfirmed body
  atomically $ do
    writeTVar bsLedgerStateVar newState
    writeTVar bsUtxoPreimageVar newPreimage
  saveState ctxDbPath newState newPreimage
  recordBatchDb ctxDbPath ids (Text.pack (show submittedTxId))
  pure submittedTxId
