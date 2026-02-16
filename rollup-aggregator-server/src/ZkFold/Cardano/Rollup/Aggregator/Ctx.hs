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

import Control.Concurrent.STM (TQueue, TVar)
import Control.Monad.Reader (ReaderT (..))
import GHC.TypeNats (type (+))
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
import ZkFold.Algebra.EllipticCurve.BLS12_381 (BLS12_381_G1_JacobianPoint)
import ZkFold.Cardano.Rollup.Aggregator.Config (BatchConfig)
import ZkFold.Cardano.Rollup.Aggregator.Types
import ZkFold.Cardano.Rollup.Types (ZKInitializedRollupBuildInfo)
import ZkFold.Data.MerkleTree (Leaves)
import ZkFold.Protocol.NonInteractiveProof (TrustedSetup)
import ZkFold.Protocol.Plonkup.Prover (PlonkupProverSecret)
import ZkFold.Symbolic.Ledger.Circuit.Compile (LedgerCircuit, LedgerCircuitGates)
import ZkFold.Symbolic.Ledger.Types

-- | Server context containing all shared state and configuration.
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
  , ctxBatchQueue ∷ !(TQueue QueuedTx)
  -- ^ Batch queue.
  , ctxLedgerStateVar ∷ !(TVar (State Bi Bo Ud A I))
  -- ^ Current ledger state.
  , ctxUtxoPreimageVar ∷ !(TVar (Leaves Ud (UTxO A I)))
  -- ^ UTxO preimage.
  , ctxTrustedSetup ∷ !(TrustedSetup (LedgerCircuitGates + 6))
  -- ^ Trusted setup for proof.
  , ctxLedgerCircuit ∷ !(LedgerCircuit Bi Bo Ud A Ixs Oxs TxCount)
  -- ^ Ledger circuit.
  , ctxProverSecret ∷ !(PlonkupProverSecret BLS12_381_G1_JacobianPoint)
  -- ^ Prover secret.
  , ctxStatePersistPath ∷ !(Maybe FilePath)
  -- ^ Optional file path for persisting ledger state.
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
