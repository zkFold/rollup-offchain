-- | Chain sync client for monitoring rollup state updates.
--
-- Connects to a local Cardano node via the Ouroboros ChainSync mini-protocol.
-- ChainSync is the **single source of truth** for the Merkle tree and rollup
-- state. It maintains the tree leaf hashes from on-chain deltas and handles
-- rollbacks via an in-memory state history (up to 20 snapshots).
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
import Control.Exception (SomeException, catch)
import Control.Lens ((^.))
import Control.Monad (forM_, when)
import Data.Function ((&))
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
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
import ZkFold.Cardano.Rollup.Aggregator.Persistence (saveState)
import ZkFold.Cardano.Rollup.Aggregator.Types (A, I, N, TxCount, Ud)
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
  a ← Async.async $ chainSyncLoop ctx bs connInfo
  Async.link a
  pure a

-- | The chain sync loop. Reconnects on failure.
chainSyncLoop ∷ Ctx → BatcherState → Api.LocalNodeConnectInfo → IO ()
chainSyncLoop ctx bs connInfo = do
  gyLogInfo (ctxProviders ctx) mempty "Chain sync: connecting to local node"
  Api.connectToLocalNode connInfo protocols
    `catch` \(e ∷ SomeException) → do
      gyLogError (ctxProviders ctx) mempty $
        "Chain sync: connection error: " <> show e <> ", reconnecting in 5s"
      threadDelay 5_000_000
      chainSyncLoop ctx bs connInfo
 where
  protocols =
    Api.LocalNodeClientProtocols
      { localChainSyncClient = Api.LocalChainSyncClient $ chainSyncClient ctx bs
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }

-- | The ChainSync protocol client.
-- Starts from genesis (or a resume point) and processes each block.
chainSyncClient
  ∷ Ctx
  → BatcherState
  → Api.ChainSyncClient Api.BlockInMode Api.ChainPoint Api.ChainTip IO ()
chainSyncClient ctx bs =
  Api.ChainSyncClient $ pure initialise
 where
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
          pure requestNext
      , Api.Sync.recvMsgRollBackward = \point _tip → Api.ChainSyncClient $ do
          handleRollback ctx bs point
          pure requestNext
      }

-- | Process a new block: scan for rollup state updates.
-- ChainSync is the single writer for the Merkle tree and state TVars.
handleRollForward ∷ Ctx → BatcherState → Api.BlockInMode → IO ()
handleRollForward ctx bs (Api.BlockInMode Api.ConwayEra (Api.Block header txs)) = do
  let Api.BlockHeader (Api.SlotNo slot) _ _ = header
      nft = zkirbiNFT (ctxRollupBuildInfo ctx)
      updates = mapMaybe (findRollupUpdate nft) txs
  forM_ updates $ \RollupStateUpdate {..} → do
    currentState ← readTVarIO (bsLedgerStateVar bs)
    let localChainLen = feToInteger (sLength currentState)
        onChainLen = chainLength rsuNewRollupState
        onChainRoot = utxoTreeRoot rsuNewRollupState
        localRoot = feToInteger (sUTxO currentState)
    -- Process this update if it advances beyond our current state,
    -- or if it represents a different state at the same length (fork).
    -- During replay from genesis, old blocks (onChainLen <= localChainLen
    -- with matching root) are skipped because the loaded state already
    -- reflects them.
    when (onChainLen > localChainLen || (onChainLen == localChainLen && onChainRoot /= localRoot)) $ do
      gyLogInfo (ctxProviders ctx) mempty $
        "Chain sync: applying state update (chain len " <> show localChainLen
          <> " → " <> show onChainLen <> ", slot " <> show slot <> ")"
      applyRollupUpdate ctx bs slot rsuNewRollupState rsuDelta
handleRollForward _ _ _ = pure () -- Ignore non-Conway blocks.

-- | Handle a chain rollback by restoring from the in-memory state history.
handleRollback ∷ Ctx → BatcherState → Api.ChainPoint → IO ()
handleRollback ctx bs point = do
  let targetSlot = case point of
        Api.ChainPointAtGenesis → 0
        Api.ChainPoint (Api.SlotNo s) _ → s
  gyLogWarning (ctxProviders ctx) mempty $
    "Chain sync: rollback to slot " <> show targetSlot
  atomically $ do
    history ← readTVar (bsStateHistoryVar bs)
    -- Find the most recent snapshot at or before the target slot.
    case dropWhile (\(s, _, _) → s > targetSlot) history of
      ((_, st, lh) : rest) → do
        writeTVar (bsLedgerStateVar bs) st
        writeTVar (bsLeafHashesVar bs) lh
        writeTVar (bsMerkleTreeVar bs) (SymMerkle.fromLeaves lh)
        writeTVar (bsStateHistoryVar bs) rest
      [] → do
        -- No snapshot covers this slot; reset to initial state.
        let initLH = pure (nullUTxOHash @A @I)
        writeTVar (bsLedgerStateVar bs) initialState
        writeTVar (bsLeafHashesVar bs) initLH
        writeTVar (bsMerkleTreeVar bs) (SymMerkle.fromLeaves initLH)
        writeTVar (bsStateHistoryVar bs) []

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
      Api.TxBody content = txBody
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
              , rsuDelta = maybe [] id extractDelta
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
  (currentState, currentLeafHashes) ← atomically $
    (,) <$> readTVar (bsLedgerStateVar bs) <*> readTVar (bsLeafHashesVar bs)

  -- Extract (position, newHash) from delta and apply to leaf hashes.
  let modifiedLeaves = collectModifiedLeaves biCount txCount nCount delta
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
  gyLogInfo (ctxProviders ctx) mempty $
    "Chain sync: state updated, " <> show (length modifiedLeaves) <> " leaf positions modified"

-- | Extract (position, newLeafHash) pairs from the flat delta list.
--
-- Delta structure: @[bi*(pos,hash)] ++ [t*n*pos] ++ [t*n*(isActive,pos,hash)]@
--
-- For inputs (consumed positions), the new hash is 'nullUTxOHash'.
-- For bridge-ins and active outputs, the new hash is given in the delta.
-- Outputs and bridge-ins override inputs at the same position.
collectModifiedLeaves
  ∷ Int
  -- ^ Bridge-in count (Bi).
  → Int
  -- ^ Transaction count (TxCount).
  → Int
  -- ^ Inputs/outputs per transaction (N).
  → [Integer]
  -- ^ Flat delta list.
  → [(Integer, FieldElement I)]
collectModifiedLeaves biCount txCount nCount delta =
  Map.toList finalMap
 where
  (biPart, rest1) = splitAt (biCount * 2) delta
  (inputPart, rest2) = splitAt (txCount * nCount) rest1
  outputPart = rest2

  nullHash ∷ FieldElement I
  nullHash = nullUTxOHash @A @I

  -- Inputs: consumed positions → nullUTxOHash.
  inputMap = Map.fromList [(pos, nullHash) | pos ← inputPart]

  -- Bridge-ins: (pos, hash) pairs.
  biMap = Map.fromList (pairUpFE biPart)

  -- Active outputs: (isActive, pos, hash) triples → (pos, hash) for active.
  outMap = Map.fromList (activeOutputLeaves outputPart)

  -- Outputs override bridge-ins override inputs.
  finalMap = outMap `Map.union` biMap `Map.union` inputMap

  pairUpFE ∷ [Integer] → [(Integer, FieldElement I)]
  pairUpFE (pos : h : rest) = (pos, fromConstant h) : pairUpFE rest
  pairUpFE _ = []

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
