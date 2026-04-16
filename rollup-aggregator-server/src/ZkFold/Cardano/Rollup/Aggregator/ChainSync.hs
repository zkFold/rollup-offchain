-- | Chain sync client for monitoring rollup state updates.
--
-- Connects to a local Cardano node via the Ouroboros ChainSync mini-protocol.
-- ChainSync is the **single source of truth** for the Merkle tree and rollup
-- state. It maintains the tree leaf hashes from on-chain deltas and handles
-- rollbacks via a state history (up to 20 snapshots, persisted to SQLite).
--
-- The Batcher communicates preimage data (full UTxO objects for leaf hashes it
-- created) via the @utxo_preimages@ SQLite table — an append-only store keyed
-- by leaf hash. ChainSync never reads or writes preimages; it operates
-- exclusively on hashes.
module ZkFold.Cardano.Rollup.Aggregator.ChainSync (
  startChainSync,
) where

import Cardano.Api qualified as Api
import Cardano.Api.ChainSync.Client qualified as Api.Sync
import Cardano.Api.Shelley qualified as Api.S
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Plutus.Data qualified as Ledger
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (atomically, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeAsyncException, SomeException, catch, fromException, throwIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Time.Clock (getCurrentTime)
import Control.Lens ((^.))
import Control.Monad (forM_)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Word (Word64)
import GeniusYield.Providers.Node (networkIdToLocalNodeConnectInfo)
import GeniusYield.Types (
  GYNonAdaToken (..),
  gyLogError,
  gyLogInfo,
  gyLogWarning,
  mintingPolicyIdToApi,
  tokenNameToApi,
 )
import PlutusTx qualified
import ZkFold.Algebra.Class (FromConstant (..))
import ZkFold.Cardano.Rollup.Aggregator.Batcher (BatcherState (..), initialState)
import ZkFold.Cardano.Rollup.Aggregator.Ctx (Ctx (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import ZkFold.Cardano.Rollup.Aggregator.Config (ChainSyncStartPoint (..))
import ZkFold.Cardano.Rollup.Aggregator.Persistence (loadCheckpoint, saveResetToGenesisDb, saveRollbackDb, saveRollupUpdateDb)
import ZkFold.Cardano.Rollup.Aggregator.Types (A, I, Ud)
import ZkFold.Cardano.Rollup.Api.Utils (feToInteger)
import ZkFold.Cardano.Rollup.Types (ZKInitializedRollupBuildInfo (..))
import ZkFold.Cardano.UPLC.RollupSimple.Types (RollupSimpleRed (..), RollupState (..))
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Data.Vector (fromVector, unsafeToVector)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.MerkleTree qualified as SymMerkle
import ZkFold.Symbolic.Ledger.Types (State (..), nullUTxOHash)

-- | Data extracted from an on-chain rollup state update.
data RollupStateUpdate = RollupStateUpdate
  { rsuNewRollupState ∷ !RollupState
  , rsuDelta ∷ ![Integer]
  }

-- | Mutable retry state shared between the sync loop and protocol callbacks.
data RetryState = RetryState
  { rsBackoffMicros ∷ !(IORef Int)
  , rsConsecFailures ∷ !(IORef Int)
  }

-- | Mutable chain sync state for checkpoint management.
data SyncState = SyncState
  { ssCheckpointRef ∷ !(IORef Api.ChainPoint)
  -- ^ The current safe checkpoint (persisted to DB, used for FindIntersect).
  , ssRecentBlocksRef ∷ !(IORef [(Word64, Api.Hash Api.BlockHeader)])
  -- ^ Ring buffer of (slot, blockHash) from recent rollup updates.
  -- Most recent first, capped at 'checkpointDepth'. The oldest entry
  -- becomes the next checkpoint once the buffer is full.
  }

-- | How many rollup updates behind the tip the checkpoint should lag.
-- This ensures the checkpoint points to a block deep enough to be
-- effectively immune to rollbacks.
checkpointDepth ∷ Int
checkpointDepth = 20

-- | Start the chain sync client as a background thread.
-- Returns the async handle (linked to the calling thread so exceptions propagate).
startChainSync
  ∷ Ctx
  → BatcherState
  → FilePath
  -- ^ Cardano node socket path.
  → Maybe ChainSyncStartPoint
  -- ^ Optional starting point used only on first run (no persisted checkpoint).
  -- Lets the operator skip syncing from genesis when the rollup was deployed recently.
  → IO (Async.Async ())
startChainSync ctx bs socketPath mStartPoint = do
  let connInfo = networkIdToLocalNodeConnectInfo (ctxNetworkId ctx) socketPath
  rs ← RetryState <$> newIORef initialBackoffMicros <*> newIORef 0
  -- Load persisted checkpoint for resuming chain sync after restart.
  mCheckpoint ← loadCheckpoint (ctxDbPath ctx)
  resumePoint ← case mCheckpoint of
    Just (slotNo, hashHex) →
      case Api.deserialiseFromRawBytesHex (Api.AsHash (Api.proxyToAsType (Proxy @Api.BlockHeader))) (Text.encodeUtf8 hashHex) of
        Right blockHash → do
          gyLogInfo (ctxProviders ctx) mempty $
            "Chain sync: resuming from checkpoint at slot " <> show slotNo
          pure $ Api.ChainPoint (Api.SlotNo slotNo) blockHash
        Left err → do
          gyLogWarning (ctxProviders ctx) mempty $
            "Chain sync: failed to decode persisted checkpoint: " <> show err <> ", starting from genesis"
          pure Api.ChainPointAtGenesis
    Nothing →
      -- No checkpoint yet — first run. Try to use the configured start point so
      -- we skip syncing all blocks prior to rollup deployment.
      case mStartPoint of
        Nothing → do
          gyLogInfo (ctxProviders ctx) mempty "Chain sync: no checkpoint found, starting from genesis"
          pure Api.ChainPointAtGenesis
        Just ChainSyncStartPoint {..} →
          case Api.deserialiseFromRawBytesHex (Api.AsHash (Api.proxyToAsType (Proxy @Api.BlockHeader))) (Text.encodeUtf8 csspBlockHash) of
            Right blockHash → do
              gyLogInfo (ctxProviders ctx) mempty $
                "Chain sync: no checkpoint found, starting from configured start point at slot " <> show csspSlot
              pure $ Api.ChainPoint (Api.SlotNo csspSlot) blockHash
            Left err → do
              gyLogWarning (ctxProviders ctx) mempty $
                "Chain sync: failed to decode configured start point block hash: " <> show err <> ", starting from genesis"
              pure Api.ChainPointAtGenesis
  checkpointRef ← newIORef resumePoint
  recentBlocksRef ← newIORef []
  let ss = SyncState checkpointRef recentBlocksRef
  a ← Async.async $ chainSyncLoop ctx bs connInfo rs ss
  Async.link a
  pure a

-- | Initial and maximum backoff for reconnection (in microseconds).
initialBackoffMicros, maxBackoffMicros ∷ Int
initialBackoffMicros = 5_000_000 -- 5 seconds
maxBackoffMicros = 300_000_000 -- 5 minutes

-- | Maximum consecutive failures before giving up and letting the exception
-- propagate (crashing the process so the process manager can restart it).
maxConsecFailures ∷ Int
maxConsecFailures = 5

-- | The chain sync loop. Reconnects on recoverable failure with exponential backoff.
--
-- Re-throws immediately on:
--   * Async exceptions (ThreadKilled, cancel, linked exceptions) — so that
--     'Async.link' and graceful shutdown work correctly.
--   * More than 'maxConsecFailures' consecutive failures without a single
--     successful block — likely a permanent error (logic bug, DB corruption).
chainSyncLoop ∷ Ctx → BatcherState → Api.LocalNodeConnectInfo → RetryState → SyncState → IO ()
chainSyncLoop ctx bs connInfo rs ss = do
  gyLogInfo (ctxProviders ctx) mempty "Chain sync: connecting to local node"
  Api.connectToLocalNode connInfo protocols
    `catch` \(e ∷ SomeException) →
      -- Re-throw async exceptions (ThreadKilled, cancel, linked exceptions).
      case fromException @SomeAsyncException e of
        Just _ → throwIO e
        Nothing → do
          failures ← readIORef (rsConsecFailures rs)
          let failures' = failures + 1
          writeIORef (rsConsecFailures rs) failures'
          if failures' >= maxConsecFailures
            then do
              gyLogError (ctxProviders ctx) mempty $
                "Chain sync: " <> show failures' <> " consecutive failures, giving up. "
                  <> "Last error: " <> show e
              throwIO e
            else do
              backoff ← readIORef (rsBackoffMicros rs)
              let backoffSecs = backoff `div` 1_000_000
              gyLogError (ctxProviders ctx) mempty $
                "Chain sync: error (" <> show failures' <> "/" <> show maxConsecFailures <> "): "
                  <> show e <> ", reconnecting in " <> show backoffSecs <> "s"
              threadDelay backoff
              writeIORef (rsBackoffMicros rs) (min maxBackoffMicros (backoff * 2))
              chainSyncLoop ctx bs connInfo rs ss
 where
  protocols =
    Api.LocalNodeClientProtocols
      { localChainSyncClient = Api.LocalChainSyncClient $ chainSyncClient ctx bs rs ss
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

-- | The ChainSync protocol client.
-- Starts from a persisted checkpoint (or genesis on first run).
-- On reconnect, reads the latest checkpoint from the IORef.
chainSyncClient
  ∷ Ctx
  → BatcherState
  → RetryState
  → SyncState
  → Api.ChainSyncClient Api.BlockInMode Api.ChainPoint Api.ChainTip IO ()
chainSyncClient ctx bs rs ss =
  Api.ChainSyncClient $ do
    resumePoint ← readIORef (ssCheckpointRef ss)
    let intersectPoints = case resumePoint of
          Api.ChainPointAtGenesis → [Api.ChainPointAtGenesis]
          cp → [cp, Api.ChainPointAtGenesis] -- Fall back to genesis if checkpoint not on chain.
    pure (initialise intersectPoints)
 where
  -- | Bump the liveness timestamp and reset retry state after successful processing.
  touch ∷ IO ()
  touch = do
    now ← getCurrentTime
    atomically $ writeTVar (bsChainSyncAliveVar bs) now
    writeIORef (rsBackoffMicros rs) initialBackoffMicros
    writeIORef (rsConsecFailures rs) 0

  initialise intersectPoints =
    Api.Sync.SendMsgFindIntersect intersectPoints $
      Api.Sync.ClientStIntersect
        { Api.Sync.recvMsgIntersectFound = \point _tip → Api.ChainSyncClient $ do
            gyLogInfo (ctxProviders ctx) mempty $
              "Chain sync: intersection found at " <> show point
            pure requestNext
        , Api.Sync.recvMsgIntersectNotFound = \_tip → Api.ChainSyncClient $ do
            gyLogWarning (ctxProviders ctx) mempty
              "Chain sync: no intersection found, syncing from genesis"
            pure requestNext
        }

  requestNext =
    Api.Sync.SendMsgRequestNext (pure ()) handleNext

  handleNext =
    Api.Sync.ClientStNext
      { Api.Sync.recvMsgRollForward = \block _tip → Api.ChainSyncClient $ do
          handleRollForward ctx bs ss block
          touch
          pure requestNext
      , Api.Sync.recvMsgRollBackward = \point _tip → Api.ChainSyncClient $ do
          handleRollback ctx bs ss point
          touch
          pure requestNext
      }

-- | Process a new block: scan for rollup state updates.
-- ChainSync is the single writer for the Merkle tree and state TVars.
handleRollForward ∷ Ctx → BatcherState → SyncState → Api.BlockInMode → IO ()
handleRollForward ctx bs ss (Api.BlockInMode Api.ConwayEra block) = do
  let Api.BlockHeader (Api.SlotNo slot) blockHash _ = Api.getBlockHeader block
      txs = Api.getBlockTxs block
      nft = zkirbiNFT (ctxRollupBuildInfo ctx)
      updates = mapMaybe (findRollupUpdate nft) txs
  forM_ updates $ \RollupStateUpdate {..} → do
    currentState ← readTVarIO (bsLedgerStateVar bs)
    let localChainLen = feToInteger (sLength currentState)
        onChainLen = chainLength rsuNewRollupState
        onChainRoot = utxoTreeRoot rsuNewRollupState
        localRoot = feToInteger (sUTxO currentState)
        isNext = onChainLen == localChainLen + 1
        isFork = onChainLen == localChainLen && onChainRoot /= localRoot
        isAlreadySeen = onChainLen <= localChainLen && onChainRoot == localRoot
        isGap = onChainLen > localChainLen + 1
    if
      | isNext || isFork → do
          gyLogInfo (ctxProviders ctx) mempty $
            "Chain sync: applying state update (chain len " <> show localChainLen
              <> " → " <> show onChainLen <> ", slot " <> show slot <> ")"
          -- Compute checkpoint data (if the buffer is full) before persisting,
          -- so state + history + checkpoint are saved atomically.
          mCheckpoint ← updateCheckpoint ss slot blockHash
          applyRollupUpdate ctx bs ss slot rsuNewRollupState rsuDelta mCheckpoint
      | isAlreadySeen → pure () -- Replay after restart, already processed.
      | isGap → do
          -- State discontinuity: we missed update(s). This means our persisted
          -- state is inconsistent with the chain. Reset to genesis and restart
          -- chain sync so it replays all blocks in order.
          gyLogWarning (ctxProviders ctx) mempty $
            "Chain sync: state discontinuity detected (local len "
              <> show localChainLen <> ", on-chain len " <> show onChainLen
              <> "). Resetting to genesis and resyncing."
          resetToGenesis ctx bs ss
          throwIO $ userError "Chain sync: state discontinuity, resyncing from genesis"
      | otherwise → do
          -- Unexpected: onChainLen < localChainLen but roots differ.
          -- Could be a deep fork. Reset to be safe.
          gyLogWarning (ctxProviders ctx) mempty $
            "Chain sync: unexpected state mismatch (local len "
              <> show localChainLen <> " root " <> show localRoot
              <> ", on-chain len " <> show onChainLen <> " root " <> show onChainRoot
              <> "). Resetting to genesis and resyncing."
          resetToGenesis ctx bs ss
          throwIO $ userError "Chain sync: unexpected state mismatch, resyncing from genesis"
handleRollForward _ _ _ _ = pure () -- Ignore non-Conway blocks.

-- | Push a new (slot, blockHash) onto the recent-blocks buffer. When the buffer
-- reaches 'checkpointDepth', return the oldest entry as the safe checkpoint.
-- The caller persists it atomically with the state update.
-- This ensures the checkpoint is always at least 'checkpointDepth' rollup updates
-- behind the tip, making it effectively immune to rollbacks.
updateCheckpoint ∷ SyncState → Word64 → Api.Hash Api.BlockHeader → IO (Maybe (Word64, Text))
updateCheckpoint SyncState {..} slot blockHash = do
  recent ← readIORef ssRecentBlocksRef
  let recent' = take checkpointDepth ((slot, blockHash) : recent)
  writeIORef ssRecentBlocksRef recent'
  -- Only return a checkpoint when the buffer is full — the oldest entry is the safe checkpoint.
  if length recent' >= checkpointDepth
    then do
      let (safeSlot, safeHash) = last recent'
          hashHex = Text.decodeLatin1 (Api.serialiseToRawBytesHex safeHash)
      pure (Just (safeSlot, hashHex))
    else pure Nothing

-- | Reset all state to genesis (initial state, empty tree, empty history).
-- Persists the reset to SQLite so it survives a crash.
resetToGenesis ∷ Ctx → BatcherState → SyncState → IO ()
resetToGenesis ctx bs ss = do
  let initLH = pure (nullUTxOHash @A @I)
  atomically $ do
    writeTVar (bsLedgerStateVar bs) initialState
    writeTVar (bsLeafHashesVar bs) initLH
    writeTVar (bsMerkleTreeVar bs) (SymMerkle.fromLeaves initLH)
    writeTVar (bsStateHistoryVar bs) []
  writeIORef (ssRecentBlocksRef ss) []
  writeIORef (ssCheckpointRef ss) Api.ChainPointAtGenesis
  -- Atomically clear history + checkpoint + save initial state.
  saveResetToGenesisDb (ctxDbPath ctx) initialState initLH

-- | Handle a chain rollback by restoring from the state history.
handleRollback ∷ Ctx → BatcherState → SyncState → Api.ChainPoint → IO ()
handleRollback ctx bs ss point = do
  let targetSlot = case point of
        Api.ChainPointAtGenesis → 0
        Api.ChainPoint (Api.SlotNo s) _ → s
  -- A rollback to genesis when we're already at genesis is a no-op.
  -- This happens on every fresh connect: the Ouroboros protocol sends
  -- MsgRollBackward to the intersection point (genesis) after FindIntersect.
  currentState ← readTVarIO (bsLedgerStateVar bs)
  let currentLen = feToInteger (sLength currentState)
  if targetSlot == 0 && currentLen == 0
    then pure ()
    else do
      gyLogWarning (ctxProviders ctx) mempty $
        "Chain sync: rollback to slot " <> show targetSlot
      -- Trim the recent-blocks buffer to discard entries newer than the target.
      recent ← readIORef (ssRecentBlocksRef ss)
      writeIORef (ssRecentBlocksRef ss) (filter (\(s, _) → s <= targetSlot) recent)
      history ← readTVarIO (bsStateHistoryVar bs)
      -- Find the most recent snapshot at or before the target slot.
      case dropWhile (\(s, _, _) → s > targetSlot) history of
        ((_, st, lh) : rest) → do
          atomically $ do
            writeTVar (bsLedgerStateVar bs) st
            writeTVar (bsLeafHashesVar bs) lh
            writeTVar (bsMerkleTreeVar bs) (SymMerkle.fromLeaves lh)
            writeTVar (bsStateHistoryVar bs) rest
          -- Atomically prune history and persist the restored state.
          saveRollbackDb (ctxDbPath ctx) targetSlot st lh
        [] → do
          -- No snapshot covers this slot; reset to genesis and restart chain sync
          -- so it replays all blocks from the beginning.
          gyLogWarning (ctxProviders ctx) mempty $
            "Chain sync: rollback beyond history depth, resetting to genesis and resyncing"
          resetToGenesis ctx bs ss
          throwIO $ userError "Chain sync: rollback beyond history, resyncing from genesis"

-- | Scan a single transaction for a rollup state update.
-- Returns 'Just' if the transaction produces an output with the rollup NFT.
-- Extracts the new RollupState from the inline datum and rsrDelta from the
-- withdrawal redeemer (via ShelleyTxBody pattern match).
findRollupUpdate ∷ GYNonAdaToken → Api.Tx Api.ConwayEra → Maybe RollupStateUpdate
findRollupUpdate (GYNonAdaToken nftMP nftTN) (Api.Tx txBody _) =
  let -- Convert NFT to cardano-api AssetId for comparison.
      nftAssetId = Api.AssetId (mintingPolicyIdToApi nftMP) (tokenNameToApi nftTN)

      -- Pattern match on ShelleyTxBody to access outputs and script data.
      Api.S.ShelleyTxBody _sbe _ledgerBody _scripts scriptData _mAux _sv = txBody
      content = Api.getTxBodyContent txBody
      txOuts = Api.txOuts content

      -- Find the output carrying the NFT.
      hasNft (Api.TxOut _addr val _datum _refScript) =
        Api.selectAsset (Api.txOutValueToValue val) nftAssetId == 1
      mNftOut = find hasNft txOuts

      -- Extract RollupState from the NFT output's inline datum.
      extractRollupState (Api.TxOut _addr _val datum _) =
        case datum of
          Api.TxOutDatumInline _era sd →
            PlutusTx.fromBuiltinData (PlutusTx.dataToBuiltinData (Api.S.toPlutusData (Api.getScriptData sd)))
          _ → Nothing

      -- Extract rsrDelta from any redeemer that decodes as RollupSimpleRed.
      extractDelta = case scriptData of
        Api.TxBodyNoScriptData → Nothing
        Api.TxBodyScriptData _aeo _dats reds →
          let redeemersMap = Map.toList (reds ^. Ledger.unRedeemersL)
              tryDecode (_purpose, (datVal, _exUnits)) =
                case PlutusTx.fromBuiltinData (PlutusTx.dataToBuiltinData (Ledger.getPlutusData datVal)) of
                  Just (RollupSimpleRed {rsrDelta = d}) → Just d
                  Nothing → Nothing
           in case mapMaybe tryDecode redeemersMap of
                (d : _) → Just d
                [] → Nothing

   in case (mNftOut, mNftOut >>= extractRollupState) of
        (Just _, Just newState) →
          Just
            RollupStateUpdate
              { rsuNewRollupState = newState
              , rsuDelta = fromMaybe [] extractDelta
              }
        _ → Nothing

-- | Apply a rollup state update: update leaf hashes from the delta, rebuild
-- the Merkle tree, and update the State. Saves a snapshot for rollback recovery.
-- Also persists the checkpoint (if provided) atomically with the state update.
applyRollupUpdate
  ∷ Ctx
  → BatcherState
  → SyncState
  → Word64
  -- ^ Slot number of the block containing this update.
  → RollupState
  → [Integer]
  -- ^ Tree delta.
  → Maybe (Word64, Text)
  -- ^ Optional checkpoint to persist atomically with the state update.
  → IO ()
applyRollupUpdate ctx bs ss slot newRollupState delta mCheckpoint = do
  let biCount = 1 ∷ Int -- Bi
      txCount = 2 ∷ Int -- TxCount
      nCount = 2 ∷ Int -- N

      -- Construct new State from on-chain RollupState.
      newState ∷ State I =
        State
          { sPreviousStateHash = fromConstant (previousStateHash newRollupState)
          , sUTxO = fromConstant (utxoTreeRoot newRollupState)
          , sLength = fromConstant (chainLength newRollupState)
          }

  -- Snapshot current state before modifying (for rollback).
  (currentState, currentLeafHashes) ← atomically $
    (,) <$> readTVar (bsLedgerStateVar bs) <*> readTVar (bsLeafHashesVar bs)

  -- Extract (position, newHash) from delta and apply to leaf hashes.
  -- Pass the current leaf hashes so that null/padding input positions
  -- (which point at already-null leaves) can be filtered out.
  let modifiedLeaves = collectModifiedLeaves biCount txCount nCount currentLeafHashes delta
      newLeafHashes = applyLeafUpdates modifiedLeaves currentLeafHashes
      newTree = SymMerkle.fromLeaves newLeafHashes

  -- Update TVars and save rollback snapshot.
  atomically $ do
    writeTVar (bsLedgerStateVar bs) newState
    writeTVar (bsLeafHashesVar bs) newLeafHashes
    writeTVar (bsMerkleTreeVar bs) newTree
    -- Maintain state history for rollback (most recent first, cap at 20).
    history ← readTVar (bsStateHistoryVar bs)
    writeTVar (bsStateHistoryVar bs) $
      take 20 ((slot, currentState, currentLeafHashes) : history)

  -- Persist state + history snapshot + checkpoint atomically.
  saveRollupUpdateDb (ctxDbPath ctx) newState newLeafHashes slot currentState currentLeafHashes mCheckpoint
  -- Update the in-memory checkpoint IORef only after the DB write succeeds.
  case mCheckpoint of
    Just (cpSlot, cpHash) → do
      case Api.deserialiseFromRawBytesHex (Api.AsHash (Api.proxyToAsType (Proxy @Api.BlockHeader))) (Text.encodeUtf8 cpHash) of
        Right blockHash → writeIORef (ssCheckpointRef ss) (Api.ChainPoint (Api.SlotNo cpSlot) blockHash)
        Left _ → pure () -- Should not happen since we serialised it ourselves.
    Nothing → pure ()
  gyLogInfo (ctxProviders ctx) mempty $
    "Chain sync: state updated, " <> show (length modifiedLeaves) <> " leaf positions modified"

-- | Extract (position, newLeafHash) pairs from the flat delta list.
--
-- Delta structure: @[bi*(isActive,pos,hash)] ++ [t*n*pos] ++ [t*n*(isActive,pos,hash)]@
--
-- For inputs (consumed positions), the new hash is 'nullUTxOHash'.
-- For active bridge-ins and active outputs, the new hash is given in the delta.
-- Outputs and bridge-ins override inputs at the same position.
--
-- Null\/padding inputs (from 'nullOutputRef') have meaningless positions in
-- the delta — the circuit ignores them, so we must too. We detect them by
-- checking: a real consumed input always occupies a non-null leaf, while a
-- null input points at an already-null leaf.
collectModifiedLeaves
  ∷ Int
  -- ^ Bridge-in count (Bi).
  → Int
  -- ^ Transaction count (TxCount).
  → Int
  -- ^ Inputs/outputs per transaction (N).
  → Leaves Ud (FieldElement I)
  -- ^ Current leaf hashes (for filtering null input positions).
  → [Integer]
  -- ^ Flat delta list.
  → [(Integer, FieldElement I)]
collectModifiedLeaves biCount txCount nCount currentLeafHashes delta =
  Map.toList finalMap
 where
  (biPart, rest1) = splitAt (biCount * 3) delta
  (inputPart, rest2) = splitAt (txCount * nCount) rest1
  outputPart = rest2

  nullHash ∷ FieldElement I
  nullHash = nullUTxOHash @A @I

  currentHashes = fromVector currentLeafHashes

  -- Helper: look up the current hash at a given position.
  hashAt ∷ Integer → FieldElement I
  hashAt pos
    | pos >= 0 && pos < fromIntegral (length currentHashes) =
        currentHashes !! fromInteger pos
    | otherwise = nullHash

  -- Inputs: consumed positions → nullUTxOHash, but ONLY if the position
  -- currently holds a non-null leaf (i.e. a real UTxO being consumed).
  -- Positions that are already null are from padding inputs and must be skipped.
  inputMap = Map.fromList
    [ (pos, nullHash)
    | pos ← inputPart
    , hashAt pos /= nullHash
    ]

  -- Bridge-ins: (isActive, pos, hash) triples → (pos, hash) for active.
  biMap = Map.fromList (activeOutputLeaves biPart)

  -- Active outputs: (isActive, pos, hash) triples → (pos, hash) for active.
  outMap = Map.fromList (activeOutputLeaves outputPart)

  -- Outputs override bridge-ins override inputs.
  finalMap = outMap `Map.union` biMap `Map.union` inputMap

  activeOutputLeaves ∷ [Integer] → [(Integer, FieldElement I)]
  activeOutputLeaves (active : pos : h : rest)
    | active == 1 = (pos, fromConstant h) : activeOutputLeaves rest
    | otherwise = activeOutputLeaves rest
  activeOutputLeaves _ = []

-- | Apply leaf hash updates to the leaf hash vector.
applyLeafUpdates
  ∷ [(Integer, FieldElement I)]
  → Leaves Ud (FieldElement I)
  → Leaves Ud (FieldElement I)
applyLeafUpdates updates leafHashes =
  let lst = fromVector leafHashes
      updated = foldr (\(pos, h) lh → replaceNth (fromInteger pos) h lh) lst updates
   in unsafeToVector updated

-- | Replace the element at index n in a list.
replaceNth ∷ Int → a → [a] → [a]
replaceNth _ _ [] = []
replaceNth 0 v (_ : xs) = v : xs
replaceNth n v (x : xs) = x : replaceNth (n - 1) v xs
