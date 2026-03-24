-- | Chain sync client for monitoring rollup state updates by other aggregators.
--
-- Connects to a local Cardano node via the Ouroboros ChainSync mini-protocol.
-- For each new block, scans for transactions that update the rollup state UTxO
-- (identified by the NFT). When an external update is detected, extracts the
-- tree delta from the staking withdrawal redeemer and applies it to the local
-- Merkle tree state.
module ZkFold.Cardano.Rollup.Aggregator.ChainSync (
  startChainSync,
) where

import Cardano.Api qualified as Api
import Cardano.Api.ChainSync.Client qualified as Api.Sync
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM (TVar, atomically, readTVarIO, writeTVar)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, when)
import Data.Function ((&))
import Data.List (find)
import Data.Maybe (mapMaybe)
import GeniusYield.Providers.Node (networkIdToLocalNodeConnectInfo)
import GeniusYield.Types (
  GYNonAdaToken (..),
  gyLogError,
  gyLogInfo,
  gyLogWarning,
  mintingPolicyIdToApi,
  tokenNameToApi,
 )
import Cardano.Api.Shelley qualified as Api.S
import Cardano.Ledger.Alonzo.TxWits qualified as Ledger
import Cardano.Ledger.Plutus.Data qualified as Ledger
import Control.Lens ((^.))
import Data.Map.Strict qualified as Map
import PlutusTx qualified
import ZkFold.Algebra.Class (FromConstant (..), zero)
import ZkFold.Cardano.Rollup.Aggregator.Batcher (BatcherState (..), initialState)
import ZkFold.Cardano.Rollup.Aggregator.Ctx (Ctx (..))
import ZkFold.Cardano.Rollup.Aggregator.Persistence (PersistedState (..), loadState)
import ZkFold.Cardano.Rollup.Aggregator.Types (A, Bi, Bo, I, N, TxCount, Ud)
import ZkFold.Cardano.Rollup.Api.Utils (feToInteger)
import ZkFold.Cardano.Rollup.Types (ZKInitializedRollupBuildInfo (..))
import ZkFold.Cardano.UPLC.RollupSimple.Types (RollupSimpleRed (..), RollupState (..))
import ZkFold.Algebra.Field (fromZp, toZp)
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Data.MerkleTree qualified as BaseMerkle
import ZkFold.Data.Vector (fromVector, unsafeToVector)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Data.Hash (Hash (hHash), hash)
import ZkFold.Symbolic.Data.MerkleTree qualified as SymMerkle
import ZkFold.Symbolic.Ledger.Types (State (..), UTxO, nullUTxO)

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
handleRollForward ∷ Ctx → BatcherState → Api.BlockInMode → IO ()
handleRollForward ctx bs (Api.BlockInMode Api.ConwayEra (Api.Block _header txs)) = do
  let nft = zkirbiNFT (ctxRollupBuildInfo ctx)
      updates = mapMaybe (findRollupUpdate nft) txs
  forM_ updates $ \RollupStateUpdate {..} → do
    -- Check if this is our own update or an external one.
    currentState ← readTVarIO (bsLedgerStateVar bs)
    let localChainLen = feToInteger (sLength currentState)
        onChainLen = chainLength rsuNewRollupState
    when (onChainLen > localChainLen) $ do
      gyLogWarning (ctxProviders ctx) mempty $
        "Chain sync: external state update detected (chain length "
          <> show localChainLen
          <> " → "
          <> show onChainLen
          <> "), applying delta"
      applyExternalUpdate ctx bs rsuNewRollupState rsuDelta
handleRollForward _ _ _ = pure () -- Ignore non-Conway blocks.

-- | Handle a chain rollback by reverting to the last persisted state.
-- This is safe because we only persist state after our own successful batch
-- submissions, so rolling back always returns to a known-good state.
handleRollback ∷ Ctx → BatcherState → Api.ChainPoint → IO ()
handleRollback ctx bs _point = do
  gyLogWarning (ctxProviders ctx) mempty "Chain sync: rollback detected, reloading persisted state"
  mPersisted ← loadState (ctxDbPath ctx)
  let (st, utxo) = case mPersisted of
        Just (PersistedState s u) → (s, u)
        Nothing → (initialState, initialUtxoPreimage)
      tree = SymMerkle.fromLeaves (fmap (hHash . hash) utxo)
  atomically $ do
    writeTVar (bsLedgerStateVar bs) st
    writeTVar (bsUtxoPreimageVar bs) utxo
    writeTVar (bsMerkleTreeVar bs) tree
    writeTVar (bsExternalUpdateVar bs) True
 where
  initialUtxoPreimage = pure (nullUTxO @A @I)

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

-- | Apply an external state update to the local state.
-- Updates the Merkle tree leaf hashes from the delta, marks affected preimage
-- entries as unknown (nullUTxO), and signals the batcher.
applyExternalUpdate
  ∷ Ctx
  → BatcherState
  → RollupState
  → [Integer]
  -- ^ Tree delta (may be empty if redeemer extraction not yet available).
  → IO ()
applyExternalUpdate ctx bs newRollupState delta = do
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

  -- Apply the external update.
  -- The delta contains leaf hashes, but the preimage stores full UTxO objects.
  -- For positions modified by the other aggregator, we set preimage entries to
  -- nullUTxO (unknown) — the user must provide the full UTxO when spending it.
  -- We rebuild the tree from the preimage, which gives correct hashes for our
  -- own known entries and nullUTxOHash for unknown ones.
  oldPreimage ← readTVarIO (bsUtxoPreimageVar bs)
  let allPositions = collectModifiedPositions biCount txCount nCount delta
      newPreimage = markUnknown allPositions oldPreimage
      newTree = rebuildTreeFromPreimage newPreimage
  -- Update in-memory state only — do NOT persist to SQLite.
  -- On rollback, handleRollback calls loadState which must return our last
  -- known-good state (from our own batch), not an external update.
  atomically $ do
    writeTVar (bsLedgerStateVar bs) newState
    writeTVar (bsUtxoPreimageVar bs) newPreimage
    writeTVar (bsMerkleTreeVar bs) newTree
    writeTVar (bsExternalUpdateVar bs) True
  gyLogInfo (ctxProviders ctx) mempty $
    "Chain sync: external update applied, " <> show (length allPositions) <> " leaf positions marked unknown"

-- | Collect all leaf positions that were modified by the delta.
-- Delta structure: [bi*(pos,hash)] ++ [t*n*pos] ++ [t*n*(isActive,pos,hash)]
collectModifiedPositions
  ∷ Int
  -- ^ Bridge-in count (Bi).
  → Int
  -- ^ Transaction count (TxCount).
  → Int
  -- ^ Inputs/outputs per transaction (N).
  → [Integer]
  -- ^ Flat delta list.
  → [Integer]
collectModifiedPositions biCount txCount nCount delta =
  let (biPart, rest1) = splitAt (biCount * 2) delta
      (inputPart, rest2) = splitAt (txCount * nCount) rest1
      outputPart = rest2

      biPositions = everyNth 2 0 biPart
      inputPositions = inputPart
      outputPositions = activeOutputPositions outputPart
   in biPositions <> inputPositions <> outputPositions
 where
  everyNth _ _ [] = []
  everyNth n i (x : xs)
    | i `mod` n == 0 = x : everyNth n (i + 1) xs
    | otherwise = everyNth n (i + 1) xs

  activeOutputPositions [] = []
  activeOutputPositions (active : pos : _ : rest)
    | active == 1 = pos : activeOutputPositions rest
    | otherwise = activeOutputPositions rest
  activeOutputPositions _ = []

-- | Apply all leaf modifications to a preimage vector, then rebuild the tree.
-- This is more efficient than individual replaceAt calls when applying many changes.
rebuildTreeFromPreimage ∷ Leaves Ud (UTxO A I) → SymMerkle.MerkleTree Ud I
rebuildTreeFromPreimage pre = SymMerkle.fromLeaves (fmap (hHash . hash) pre)

-- | The null UTxO hash as a FieldElement.
nullUTxOHashFE ∷ FieldElement I
nullUTxOHashFE = hHash (hash (nullUTxO @A @I))

-- | Mark preimage entries at given positions as unknown (nullUTxO).
markUnknown ∷ [Integer] → Leaves Ud (UTxO A I) → Leaves Ud (UTxO A I)
markUnknown positions preimage =
  let lst = fromVector preimage
      updated = foldr (\pos pre → replaceNth (fromInteger pos) (nullUTxO @A @I) pre) lst positions
   in unsafeToVector updated

-- | Replace the element at index n in a list.
replaceNth ∷ Int → a → [a] → [a]
replaceNth _ _ [] = []
replaceNth 0 v (_ : xs) = v : xs
replaceNth n v (x : xs) = x : replaceNth (n - 1) v xs
