module ZkFold.Cardano.Rollup.Api (
  rollupAddress,
  seedRollup,
  registerRollupStake,
  updateRollupState,
) where

import Control.Monad (forM_, when)
import Control.Monad.Reader
import Data.Foldable (Foldable (..))
import Data.Function ((&))
import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.Examples.Limbo (limboValidatorV2)
import GeniusYield.TxBuilder
import GeniusYield.Types
import ZkFold.Algebra.Class (ToConstant (toConstant))
import ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes)
import ZkFold.Cardano.Rollup.Constants
import ZkFold.Cardano.Rollup.Types
import ZkFold.Cardano.UPLC.RollupSimple.Types (
  BridgeUtxoInfo (..),
  BridgeUtxoStatus (BridgeIn),
  RollupSimpleRed (..),
  RollupState,
 )
import ZkFold.Protocol.Plonkup.OffChain.Cardano
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)

-- | Get the rollup address.
rollupAddress ∷ ZKRollupQueryMonad m ⇒ m GYAddress
rollupAddress = do
  zkirbiRollup ← asks zkirbiRollup
  zkirbiStakeCred ← asks zkirbiStakeCred
  rollupAddress' zkirbiRollup zkirbiStakeCred

rollupAddress' ∷ GYTxQueryMonad m ⇒ GYScript 'PlutusV3 → Maybe (GYCredential 'GYKeyRoleStaking) → m GYAddress
rollupAddress' rollupScript mstakeCred = do
  nid ← networkId
  pure $ addressFromCredential nid (GYPaymentCredentialByScript $ scriptHash rollupScript) mstakeCred

seedRollup
  ∷ GYTxBuilderMonad m
  ⇒ ZKSetupBytes
  → Natural
  -- ^ Max bridge in outputs.
  → Natural
  -- ^ Max bridge out outputs.
  → Natural
  -- ^ Max output assets.
  → Maybe (GYCredential 'GYKeyRoleStaking)
  → RollupState
  → m (ZKInitializedRollupBuildInfo, GYTxBody)
seedRollup setup maxBridgeIn maxBridgeOut maxOutputAssets mstakeCred initialState = do
  (nft, mintTokenSkel) ← mintTestTokens "zkFold-rollup-nft" 1
  let (nftMP, nftTN) = case nonAdaTokenFromAssetClass nft of
        Nothing → error "seedRollup: absurd, minted token cannot be lovelace"
        Just (GYNonAdaToken nftMP' nftTN') → (nftMP', nftTN')
      stakeValConfig =
        ZKRollupStakeValConfig
          { zkrsvcMaxOutputAssets = maxOutputAssets
          , zkrsvcMaxBridgeOut = maxBridgeOut
          , zkrsvcMaxBridgeIn = maxBridgeIn
          , zkrsvcSetupBytes = setup
          , zkrsvcNFT = GYNonAdaToken nftMP nftTN
          }
      rollupStakeScript =
        zkrbiRollupStake zkRollupBuildInfo stakeValConfig
      rollupStakeScriptHash = scriptHash rollupStakeScript
      rollupScript = zkrbiRollup zkRollupBuildInfo rollupStakeScriptHash
  rollupAddr ← rollupAddress' rollupScript mstakeCred
  alwaysFailAddr ← scriptAddress limboValidatorV2
  txBody ←
    buildTxBody @'PlutusV2 $
      mustHaveOutput
        ( GYTxOut
            { gyTxOutAddress = alwaysFailAddr
            , gyTxOutDatum = Nothing
            , gyTxOutRefS = Just (GYPlutusScript rollupScript)
            , gyTxOutValue = mempty
            }
        )
        <> mustHaveOutput
          ( GYTxOut
              { gyTxOutAddress = alwaysFailAddr
              , gyTxOutDatum = Nothing
              , gyTxOutRefS = Just (GYPlutusScript rollupStakeScript)
              , gyTxOutValue = mempty
              }
          )
        <> mustHaveOutput
          ( GYTxOut
              { gyTxOutAddress = rollupAddr
              , gyTxOutDatum = Just (datumFromPlutusData initialState, GYTxOutUseInlineDatum)
              , gyTxOutRefS = Nothing
              , gyTxOutValue = valueSingleton nft 1
              }
          )
        <> mintTokenSkel
  let txId = txBodyTxId txBody
      scriptRef = txOutRefFromTuple (txId, 0)
      stakeScriptRef = txOutRefFromTuple (txId, 1)
      initializedBuildInfo =
        ZKInitializedRollupBuildInfo
          { zkirbiStakeCred = mstakeCred
          , zkirbiRollupRef = scriptRef
          , zkirbiRollup = rollupScript
          , zkirbiNFT = GYNonAdaToken nftMP nftTN
          , zkirbiRollupStake = rollupStakeScript
          , zkirbiRollupStakeRef = stakeScriptRef
          , zkirbiRollupStakeValConfig = stakeValConfig
          }
  pure (initializedBuildInfo, txBody)

-- | Register the rollup stake validator.
registerRollupStake ∷ ZKRollupQueryMonad m ⇒ m (GYTxSkeleton 'PlutusV3)
registerRollupStake = do
  zkirbiRollupStake ← asks zkirbiRollupStake
  zkirbiRollupStakeRef ← asks zkirbiRollupStakeRef
  pure $
    mustHaveCertificate
      ( mkStakeAddressRegistrationCertificate
          (GYCredentialByScript $ scriptHash zkirbiRollupStake)
          (GYTxBuildWitnessPlutusScript (GYBuildPlutusScriptReference zkirbiRollupStakeRef zkirbiRollupStake) unitRedeemer)
      )

-- | Update the rollup state.
updateRollupState
  ∷ ZKRollupQueryMonad m
  ⇒ RollupState
  → [(GYValue, FieldElement RollupBFInterpreter)]
  -- ^ Value to bridge in along with layer 2 address which will hold this value.
  → ProofBytes
  → m (GYTxSkeleton 'PlutusV3)
updateRollupState newState bridgeIns' proofBytes = do
  ZKInitializedRollupBuildInfo {..} ← ask
  -- We reverse the given list as in plutus validator, when it traverses the outputs, it adds the later item in list to the beginning of it's own internal list.
  let bridgeIns = reverse bridgeIns'
  forM_ bridgeIns $ \(value, _) →
    when (fromIntegral (valueTotalAssets value) > zkrsvcMaxOutputAssets zkirbiRollupStakeValConfig) $
      throwAppError $
        ZKREValueHasMoreThanMaxAssets value (fromIntegral (zkrsvcMaxOutputAssets zkirbiRollupStakeValConfig))
  rollupAddr ← rollupAddress
  nid ← networkId
  let stakeCred = GYCredentialByScript $ scriptHash zkirbiRollupStake
      stakeAddr = stakeAddressFromCredential nid stakeCred
  si ←
    stakeAddressInfo stakeAddr >>= \case
      Just si → pure si
      Nothing → throwAppError $ ZKREStakeAddressInfoNotFound stakeAddr
  rollupUTxO ←
    utxosAtAddress rollupAddr (Just $ nonAdaTokenToAssetClass zkirbiNFT) >>= \utxos → case utxosToList utxos of
      [utxo] → pure utxo
      _anyOther → throwAppError $ ZKREStateUTxONotFound rollupAddr zkirbiNFT
  let newOut ∷ GYTxOut 'PlutusV3 =
        GYTxOut
          { gyTxOutValue = utxoValue rollupUTxO
          , gyTxOutRefS = Nothing
          , gyTxOutDatum = Just (datumFromPlutusData newState, GYTxOutUseInlineDatum)
          , gyTxOutAddress = rollupAddr
          }

  pure $
    mustHaveInput
      ( GYTxIn
          { gyTxInTxOutRef = utxoRef rollupUTxO
          , gyTxInWitness =
              GYTxInWitnessScript
                (GYBuildPlutusScriptReference zkirbiRollupRef zkirbiRollup)
                Nothing
                unitRedeemer
          }
      )
      <> mustHaveOutput newOut
      <> foldMap'
        ( \(val, layer2Addr) →
            mustHaveOutput $
              GYTxOut
                { gyTxOutValue = val
                , gyTxOutRefS = Nothing
                , gyTxOutDatum =
                    Just
                      ( datumFromPlutusData $
                          BridgeUtxoInfo
                            { buiStatus = BridgeIn $ fromIntegral @Natural @Integer $ toConstant $ toConstant layer2Addr
                            , buiORef = utxoRef rollupUTxO & txOutRefToPlutusV3
                            }
                      , GYTxOutUseInlineDatum
                      )
                , gyTxOutAddress = rollupAddr
                }
        )
        bridgeIns
      <> mustHaveWithdrawal
        ( GYTxWdrl
            { gyTxWdrlStakeAddress = stakeAddr
            , gyTxWdrlAmount = gyStakeAddressInfoAvailableRewards si
            , gyTxWdrlWitness =
                GYTxBuildWitnessPlutusScript
                  (GYBuildPlutusScriptReference zkirbiRollupStakeRef zkirbiRollupStake)
                  ( redeemerFromPlutusData $
                      RollupSimpleRed
                        { rsrProofBytes =
                            proofBytes
                        , rsrAddress = addressToPlutus rollupAddr
                        }
                  )
            }
        )