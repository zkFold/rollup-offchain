module ZkFold.Cardano.Rollup.Aggregator.Ctx (
  -- * Server Context
  Ctx (..),

  -- * Context Operations
  logInfo,
  logDebug,
  logWarn,
  logError,

  -- * TxBuilder & Query Functions
  runSkeletonI,
  runSkeletonWithStrategyI,
  runSkeletonF,
  runSkeletonWithStrategyF,
  runQuery,
  runQueryWithReader,
  runGYTxMonadNodeF,
) where

import Control.Monad.Reader (ReaderT (..))
import GeniusYield.Imports (HasCallStack, Identity (..), coerce)
import GeniusYield.Transaction (GYCoinSelectionStrategy (..))
import GeniusYield.TxBuilder
import GeniusYield.Types (
  GYAddress,
  GYNetworkId,
  GYProviders,
  GYSomePaymentSigningKey,
  GYTxBody,
  GYTxOutRef,
  gyLogDebug,
  gyLogError,
  gyLogInfo,
  gyLogWarning,
 )
import ZkFold.Cardano.Rollup.Aggregator.Config (BatchConfig)
import ZkFold.Cardano.Rollup.Types (ZKInitializedRollupBuildInfo)

-- | Server context containing shared configuration for both the server and batcher.
data Ctx = Ctx
  { ctxNetworkId ∷ !GYNetworkId
  -- ^ Cardano network ID.
  , ctxProviders ∷ !GYProviders
  -- ^ Atlas providers for L1 interaction.
  , ctxSigningKey ∷ !(GYSomePaymentSigningKey, GYAddress)
  -- ^ Signing key.
  , ctxCollateral ∷ !GYTxOutRef
  -- ^ Collateral UTxO.
  , ctxRollupBuildInfo ∷ !ZKInitializedRollupBuildInfo
  -- ^ Rollup script information.
  , ctxBatchConfig ∷ !BatchConfig
  -- ^ Batch processing configuration.
  , ctxDbPath ∷ !FilePath
  -- ^ SQLite database file path shared between server and batcher processes.
  }

logDebug ∷ HasCallStack ⇒ Ctx → String → IO ()
logDebug ctx = gyLogDebug (ctxProviders ctx) mempty

logInfo ∷ HasCallStack ⇒ Ctx → String → IO ()
logInfo ctx = gyLogInfo (ctxProviders ctx) mempty

logWarn ∷ HasCallStack ⇒ Ctx → String → IO ()
logWarn ctx = gyLogWarning (ctxProviders ctx) mempty

logError ∷ HasCallStack ⇒ Ctx → String → IO ()
logError ctx = gyLogError (ctxProviders ctx) mempty

-- | Create 'TxBody' from a 'GYTxSkeleton'.
runSkeletonI
  ∷ Ctx
  → [GYAddress]
  -- ^ User's used addresses. Note that internally we prepend given change address to this list so that in case wallet's state isn't updated quickly to mark an earlier given change address as used, we'll be able to use UTxOs potentially present at this change address.
  → GYAddress
  -- ^ User's change address.
  → Maybe GYTxOutRef
  -- ^ User's collateral.
  → ReaderT ZKInitializedRollupBuildInfo GYTxBuilderMonadIO (GYTxSkeleton v)
  → IO GYTxBody
runSkeletonI = coerce (runSkeletonF @Identity)

-- | Create 'TxBody' from a 'GYTxSkeleton', with the specified coin selection strategy.
runSkeletonWithStrategyI
  ∷ GYCoinSelectionStrategy
  → Ctx
  → [GYAddress]
  -- ^ User's used addresses. Note that internally we prepend given change address to this list so that in case wallet's state isn't updated quickly to mark an earlier given change address as used, we'll be able to use UTxOs potentially present at this change address.
  → GYAddress
  -- ^ User's change address.
  → Maybe GYTxOutRef
  -- ^ User's collateral.
  → ReaderT ZKInitializedRollupBuildInfo GYTxBuilderMonadIO (GYTxSkeleton v)
  → IO GYTxBody
runSkeletonWithStrategyI cstrat = coerce (runSkeletonWithStrategyF @Identity cstrat)

runSkeletonF
  ∷ Traversable t
  ⇒ Ctx
  → [GYAddress]
  -- ^ User's used addresses. Note that internally we prepend given change address to this list so that in case wallet's state isn't updated quickly to mark an earlier given change address as used, we'll be able to use UTxOs potentially present at this change address.
  → GYAddress
  -- ^ User's change address.
  → Maybe GYTxOutRef
  -- ^ User's collateral.
  → ReaderT ZKInitializedRollupBuildInfo GYTxBuilderMonadIO (t (GYTxSkeleton v))
  → IO (t GYTxBody)
runSkeletonF = runSkeletonWithStrategyF GYRandomImproveMultiAsset

runSkeletonWithStrategyF
  ∷ Traversable t
  ⇒ GYCoinSelectionStrategy
  → Ctx
  → [GYAddress]
  -- ^ User's used addresses. Note that internally we prepend given change address to this list so that in case wallet's state isn't updated quickly to mark an earlier given change address as used, we'll be able to use UTxOs potentially present at this change address.
  → GYAddress
  -- ^ User's change address.
  → Maybe GYTxOutRef
  -- ^ User's collateral.
  → ReaderT ZKInitializedRollupBuildInfo GYTxBuilderMonadIO (t (GYTxSkeleton v))
  → IO (t GYTxBody)
runSkeletonWithStrategyF cstrat ctx addrs addr mcollateral skeleton = do
  let nid = ctxNetworkId ctx
      providers = ctxProviders ctx
      di = ctxRollupBuildInfo ctx
      mcollateral' = do
        collateral ← mcollateral
        pure (collateral, False)

  runGYTxMonadNodeF cstrat nid providers (addr : addrs) addr mcollateral' $ runReaderT skeleton di

runQuery ∷ Ctx → ReaderT ZKInitializedRollupBuildInfo GYTxQueryMonadIO a → IO a
runQuery ctx = runQueryWithReader ctx (ctxRollupBuildInfo ctx)

runQueryWithReader ∷ Ctx → a → ReaderT a GYTxQueryMonadIO b → IO b
runQueryWithReader ctx a q = do
  let nid = ctxNetworkId ctx
      providers = ctxProviders ctx
  runGYTxQueryMonadIO nid providers $ runReaderT q a

runGYTxMonadNodeF
  ∷ ∀ t v
   . Traversable t
  ⇒ GYCoinSelectionStrategy
  → GYNetworkId
  → GYProviders
  → [GYAddress]
  → GYAddress
  → Maybe (GYTxOutRef, Bool)
  → GYTxBuilderMonadIO (t (GYTxSkeleton v))
  → IO (t GYTxBody)
runGYTxMonadNodeF strat nid providers addrs change collateral act = runGYTxBuilderMonadIO nid providers addrs change collateral $ act >>= traverse (buildTxBodyWithStrategy strat)
