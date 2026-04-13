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
import Control.Lens ((^.))
import Control.Monad (forM_, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time.Clock (getCurrentTime)
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
import ZkFold.Cardano.Rollup.Api.Utils (feToInteger)
import ZkFold.Cardano.Rollup.Types (ZKInitializedRollupBuildInfo (..))
import ZkFold.Cardano.UPLC.RollupSimple.Types (RollupSimpleRed (..), RollupState (..))
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Data.Vector (fromVector, unsafeToVector)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.MerkleTree qualified as SymMerkle
import ZkFold.Symbolic.Ledger.Types (State (..), nullUTxOHash)

import ZkFold.Cardano.Rollup.Aggregator.Batcher (BatcherState (..), initialState)
import ZkFold.Cardano.Rollup.Aggregator.Ctx (Ctx (..))
import ZkFold.Cardano.Rollup.Aggregator.Persistence (pruneStateHistory, saveState, saveStateHistory)
import ZkFold.Cardano.Rollup.Aggregator.Types (A, I, Ud)

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

-- | Start the chain sync client as a background thread.
-- Returns the async handle (linked to the calling thread so exceptions propagate).
startChainSync
  ∷ Ctx
  → BatcherState
  → FilePath
  -- ^ Cardano node socket path.
  → IO (Async.Async ())
startChainSync ctx bs socketPath = do
  let connInfo = networkIdToLocalNodeConnectInfo (ctxNetworkId ctx) socketPath
  rs ← RetryState <$> newIORef initialBackoffMicros <*> newIORef 0
  a ← Async.async $ chainSyncLoop ctx bs connInfo rs
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
chainSyncLoop ∷ Ctx → BatcherState → Api.LocalNodeConnectInfo → RetryState → IO ()
chainSyncLoop ctx bs connInfo rs = do
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
                "Chain sync: "
                  <> show failures'
                  <> " consecutive failures, giving up. "
                  <> "Last error: "
                  <> show e
              throwIO e
            else do
              backoff ← readIORef (rsBackoffMicros rs)
              let backoffSecs = backoff `div` 1_000_000
              gyLogError (ctxProviders ctx) mempty $
                "Chain sync: error ("
                  <> show failures'
                  <> "/"
                  <> show maxConsecFailures
                  <> "): "
                  <> show e
                  <> ", reconnecting in "
                  <> show backoffSecs
                  <> "s"
              threadDelay backoff
              writeIORef (rsBackoffMicros rs) (min maxBackoffMicros (backoff * 2))
              chainSyncLoop ctx bs connInfo rs
 where
  protocols =
    Api.LocalNodeClientProtocols
      { localChainSyncClient = Api.LocalChainSyncClient $ chainSyncClient ctx bs rs
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

-- | The ChainSync protocol client.
-- Starts from genesis (or a resume point) and processes each block.
chainSyncClient
  ∷ Ctx
  → BatcherState
  → RetryState
  → Api.ChainSyncClient Api.BlockInMode Api.ChainPoint Api.ChainTip IO ()
chainSyncClient ctx bs rs =
  Api.ChainSyncClient $ pure initialise
 where
  -- \| Bump the liveness timestamp and reset retry state after successful processing.
  touch ∷ IO ()
  touch = do
    now ← getCurrentTime
    atomically $ writeTVar (bsChainSyncAliveVar bs) now
    writeIORef (rsBackoffMicros rs) initialBackoffMicros
    writeIORef (rsConsecFailures rs) 0

  initialise =
    Api.Sync.SendMsgFindIntersect [Api.ChainPointAtGenesis] $
      Api.Sync.ClientStIntersect
        { Api.Sync.recvMsgIntersectFound = \_point _tip →
            Api.ChainSyncClient $ pure requestNext
        , Api.Sync.recvMsgIntersectNotFound = \_tip →
            Api.ChainSyncClient $ pure requestNext
        }

  requestNext =
    Api.Sync.SendMsgRequestNext (pure ()) handleNext

  handleNext =
    Api.Sync.ClientStNext
      { Api.Sync.recvMsgRollForward = \block _tip → Api.ChainSyncClient $ do
          handleRollForward ctx bs block
          touch
          pure requestNext
      , Api.Sync.recvMsgRollBackward = \point _tip → Api.ChainSyncClient $ do
          handleRollback ctx bs point
          touch
          pure requestNext
      }

-- | Process a new block: scan for rollup state updates.
-- ChainSync is the single writer for the Merkle tree and state TVars.
handleRollForward ∷ Ctx → BatcherState → Api.BlockInMode → IO ()
handleRollForward ctx bs (Api.BlockInMode Api.ConwayEra block) = do
  let Api.BlockHeader (Api.SlotNo slot) _ _ = Api.getBlockHeader block
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
            "Chain sync: applying state update (chain len "
              <> show localChainLen
              <> " → "
              <> show onChainLen
              <> ", slot "
              <> show slot
              <> ")"
          applyRollupUpdate ctx bs slot rsuNewRollupState rsuDelta
      | isAlreadySeen → pure () -- Replay after restart, already processed.
      | isGap → do
          -- State discontinuity: we missed update(s). This means our persisted
          -- state is inconsistent with the chain. Reset to genesis and restart
          -- chain sync so it replays all blocks in order.
          gyLogWarning (ctxProviders ctx) mempty $
            "Chain sync: state discontinuity detected (local len "
              <> show localChainLen
              <> ", on-chain len "
              <> show onChainLen
              <> "). Resetting to genesis and resyncing."
          resetToGenesis ctx bs
          throwIO $ userError "Chain sync: state discontinuity, resyncing from genesis"
      | otherwise → do
          -- Unexpected: onChainLen < localChainLen but roots differ.
          -- Could be a deep fork. Reset to be safe.
          gyLogWarning (ctxProviders ctx) mempty $
            "Chain sync: unexpected state mismatch (local len "
              <> show localChainLen
              <> " root "
              <> show localRoot
              <> ", on-chain len "
              <> show onChainLen
              <> " root "
              <> show onChainRoot
              <> "). Resetting to genesis and resyncing."
          resetToGenesis ctx bs
          throwIO $ userError "Chain sync: unexpected state mismatch, resyncing from genesis"
handleRollForward _ _ _ = pure () -- Ignore non-Conway blocks.

-- | Reset all state to genesis (initial state, empty tree, empty history).
-- Persists the reset to SQLite so it survives a crash.
resetToGenesis ∷ Ctx → BatcherState → IO ()
resetToGenesis ctx bs = do
  let initLH = pure (nullUTxOHash @A @I)
  atomically $ do
    writeTVar (bsLedgerStateVar bs) initialState
    writeTVar (bsLeafHashesVar bs) initLH
    writeTVar (bsMerkleTreeVar bs) (SymMerkle.fromLeaves initLH)
    writeTVar (bsStateHistoryVar bs) []
  pruneStateHistory (ctxDbPath ctx) 0
  saveState (ctxDbPath ctx) initialState initLH

-- | Handle a chain rollback by restoring from the state history.
handleRollback ∷ Ctx → BatcherState → Api.ChainPoint → IO ()
handleRollback ctx bs point = do
  let targetSlot = case point of
        Api.ChainPointAtGenesis → 0
        Api.ChainPoint (Api.SlotNo s) _ → s
  gyLogWarning (ctxProviders ctx) mempty $
    "Chain sync: rollback to slot " <> show targetSlot
  history ← readTVarIO (bsStateHistoryVar bs)
  -- Find the most recent snapshot at or before the target slot.
  case dropWhile (\(s, _, _) → s > targetSlot) history of
    ((_, st, lh) : rest) → do
      atomically $ do
        writeTVar (bsLedgerStateVar bs) st
        writeTVar (bsLeafHashesVar bs) lh
        writeTVar (bsMerkleTreeVar bs) (SymMerkle.fromLeaves lh)
        writeTVar (bsStateHistoryVar bs) rest
      -- Keep DB history in sync: remove snapshots newer than the rollback target.
      pruneStateHistory (ctxDbPath ctx) targetSlot
      -- Persist the restored state so a crash after rollback doesn't lose it.
      saveState (ctxDbPath ctx) st lh
    [] → do
      -- No snapshot covers this slot; reset to genesis and restart chain sync
      -- so it replays all blocks from the beginning.
      gyLogWarning (ctxProviders ctx) mempty $
        "Chain sync: rollback beyond history depth, resetting to genesis and resyncing"
      resetToGenesis ctx bs
      throwIO $ userError "Chain sync: rollback beyond history, resyncing from genesis"

-- | Scan a single transaction for a rollup state update.
-- Returns 'Just' if the transaction produces an output with the rollup NFT.
-- Extracts the new RollupState from the inline datum and rsrDelta from the
-- withdrawal redeemer (via ShelleyTxBody pattern match).
findRollupUpdate ∷ GYNonAdaToken → Api.Tx Api.ConwayEra → Maybe RollupStateUpdate
findRollupUpdate (GYNonAdaToken nftMP nftTN) (Api.Tx txBody _) =
  let
    -- Convert NFT to cardano-api AssetId for comparison.
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
   in
    case (mNftOut, mNftOut >>= extractRollupState) of
      (Just _, Just newState) →
        Just
          RollupStateUpdate
            { rsuNewRollupState = newState
            , rsuDelta = fromMaybe [] extractDelta
            }
      _ → Nothing

-- | Apply a rollup state update: update leaf hashes from the delta, rebuild
-- the Merkle tree, and update the State. Saves a snapshot for rollback recovery.
applyRollupUpdate
  ∷ Ctx
  → BatcherState
  → Word64
  -- ^ Slot number of the block containing this update.
  → RollupState
  → [Integer]
  -- ^ Tree delta.
  → IO ()
applyRollupUpdate ctx bs slot newRollupState delta = do
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
  (currentState, currentLeafHashes) ←
    atomically $
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

  -- Persist to SQLite for crash recovery.
  saveState (ctxDbPath ctx) newState newLeafHashes
  -- Persist rollback snapshot so history survives restarts.
  saveStateHistory (ctxDbPath ctx) slot currentState currentLeafHashes
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
  inputMap =
    Map.fromList
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
