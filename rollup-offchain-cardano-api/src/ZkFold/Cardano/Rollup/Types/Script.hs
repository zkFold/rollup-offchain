module ZkFold.Cardano.Rollup.Types.Script (
  ZKRollupStakeValConfig (..),
  rollupStakeValConfigToPlutus,
  ZKRollupBuildInfo (..),
  ZKInitializedRollupBuildInfo (..),
) where

import GeniusYield.Types
import ZkFold.Cardano.UPLC.RollupSimple.Types qualified as Onchain
import ZkFold.Protocol.Plonkup.OffChain.Cardano (ZKSetupBytes)

import ZkFold.Cardano.Rollup.Utils (setupToPlutus)

data ZKRollupStakeValConfig = ZKRollupStakeValConfig
  { zkrsvcNFT ∷ GYNonAdaToken
  , zkrsvcSetupBytes ∷ ZKSetupBytes
  , zkrsvcMaxBridgeIn ∷ Natural
  , zkrsvcMaxBridgeOut ∷ Natural
  , zkrsvcMaxOutputAssets ∷ Natural
  }
  deriving stock Show

rollupStakeValConfigToPlutus ∷ ZKRollupStakeValConfig → Onchain.RollupConfiguration
rollupStakeValConfigToPlutus ZKRollupStakeValConfig {..} =
  Onchain.RollupConfiguration
    { rcNftCurrencySymbol = mintingPolicyIdToCurrencySymbol nftMP
    , rcNftTokenName = tokenNameToPlutus nftTN
    , rcSetupBytes = setupToPlutus zkrsvcSetupBytes
    , rcMaxBridgeIn = fromIntegral zkrsvcMaxBridgeIn
    , rcMaxBridgeOut = fromIntegral zkrsvcMaxBridgeOut
    , rcMaxOutputAssets = fromIntegral zkrsvcMaxOutputAssets
    }
 where
  GYNonAdaToken nftMP nftTN = zkrsvcNFT

-- | Information required to build transactions for rollup.
data ZKRollupBuildInfo = ZKRollupBuildInfo
  { zkrbiRollup ∷ GYScriptHash → GYScript 'PlutusV3
  -- ^ Rollup spend script.
  , zkrbiRollupStake ∷ ZKRollupStakeValConfig → GYScript 'PlutusV3
  -- ^ Rollup stake validator.
  }

data ZKInitializedRollupBuildInfo = ZKInitializedRollupBuildInfo
  { zkirbiRollup ∷ !(GYScript 'PlutusV3)
  -- ^ Fully applied plutus spending script of rollup.
  , zkirbiRollupStake ∷ !(GYScript 'PlutusV3)
  -- ^ Fully applied plutus stake script of rollup.
  , zkirbiNFT ∷ !GYNonAdaToken
  -- ^ State NFT.
  , zkirbiStakeCred ∷ !(Maybe (GYCredential 'GYKeyRoleStaking))
  -- ^ Stake credential that we have used for our rollup address.
  , zkirbiRollupRef ∷ !GYTxOutRef
  -- ^ Reference to the rollup script.
  , zkirbiRollupStakeRef ∷ !GYTxOutRef
  -- ^ Reference to the rollup stake script.
  , zkirbiRollupStakeValConfig ∷ !ZKRollupStakeValConfig
  -- ^ Configuration for the rollup stake validator.
  }
  deriving stock Show
