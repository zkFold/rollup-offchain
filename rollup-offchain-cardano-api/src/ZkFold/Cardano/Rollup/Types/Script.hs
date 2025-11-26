module ZkFold.Cardano.Rollup.Types.Script (
  ZKRollupBuildInfo (..),
  ZKInitializedRollupBuildInfo (..),
) where

import GeniusYield.Types
import ZkFold.Protocol.Plonkup.OffChain.Cardano (ZKSetupBytes)

-- | Information required to build transactions for rollup.
newtype ZKRollupBuildInfo = ZKRollupBuildInfo
  { zkrbiRollup ∷ ZKSetupBytes → GYMintingPolicyId → GYTokenName → GYScript 'PlutusV3
  -- ^ Rollup script.
  }

data ZKInitializedRollupBuildInfo = ZKInitializedRollupBuildInfo
  { zkirbiRollup ∷ !(GYScript 'PlutusV3)
  -- ^ Fully applied plutus script for rollup.
  , zkirbiNFT ∷ !GYNonAdaToken
  -- ^ State NFT.
  , zkirbiStakeCred ∷ !(Maybe (GYCredential 'GYKeyRoleStaking))
  -- ^ Stake credential that we have used for our rollup address.
  , zkirbiRollupRef ∷ !GYTxOutRef
  -- ^ Reference to the rollup script.
  }
  deriving stock Show
