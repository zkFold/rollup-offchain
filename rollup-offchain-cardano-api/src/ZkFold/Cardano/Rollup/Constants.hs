module ZkFold.Cardano.Rollup.Constants (
  ezkRollupBuildInfo,
  zkRollupBuildInfo,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Short qualified as SBS
import Data.Foldable (find)
import Data.Text (unpack)
import GeniusYield.Imports (Text, encodeUtf8, (&), (<&>))
import GeniusYield.Types
import PlutusTx (ToData)
import ZkFold.Cardano.UPLC.RollupSimple.CompiledScript

import ZkFold.Cardano.Rollup.Types

-- | Select a particular validator from blueprints file.
selectValScript
  ∷ (SingPlutusVersionI v, Foldable f, Show (f ValidatorBlueprint))
  ⇒ f ValidatorBlueprint
  -- ^ Validators in a blueprint file.
  → Text
  -- ^ Validator title to match against.
  → Either String (GYScript v)
selectValScript vals valTitle = do
  val ← case find (\val → validatorTitle val == valTitle) vals of
    Nothing → Left $ "Couldn't find validator \"" <> unpack valTitle <> "\" in validator blueprints: " <> show vals
    Just val → Right val
  valCC ← case validatorCompiled val of
    Nothing → Left $ "no compiled code of validator, \"" <> unpack valTitle <> "\" found"
    Just cc → Right cc
  encodeUtf8 (compiledValidatorCode valCC) & BS16.decode <&> scriptFromSerialisedScript . SBS.toShort

ezkRollupBuildInfo ∷ Either String ZKRollupBuildInfo
ezkRollupBuildInfo = do
  bp ← Aeson.eitherDecodeStrict rollupSimpleBPFile
  let vals = contractValidators bp
  rollupSimple ← selectValScript @PlutusV3 vals "rollupSimple"
  rollupSimpleStake ← selectValScript @PlutusV3 vals "rollupSimpleStake"
  pure $
    ZKRollupBuildInfo
      { zkrbiRollup = \sh →
          rollupSimple
            & fapplyParam (scriptHashToPlutus sh)
      , zkrbiRollupStake = \config →
          rollupSimpleStake
            & fapplyParam (rollupStakeValConfigToPlutus config)
      }
 where
  fapplyParam ∷ (ToData p, SingPlutusVersionI v) ⇒ p → GYScript v → GYScript v
  fapplyParam = flip applyParam

zkRollupBuildInfo ∷ ZKRollupBuildInfo
zkRollupBuildInfo = either error id ezkRollupBuildInfo
