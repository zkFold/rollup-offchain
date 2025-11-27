module ZkFold.Cardano.Rollup.Api (
  rollupAddress,
  seedRollup,
  updateRollupState,
) where

import Control.Monad.Reader
import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.Examples.Limbo (limboValidatorV2)
import GeniusYield.TxBuilder
import GeniusYield.Types
import ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes)
import ZkFold.Cardano.Rollup.Constants
import ZkFold.Cardano.Rollup.Types
import ZkFold.Cardano.UPLC.RollupSimple.Types (RollupState)
import ZkFold.Protocol.Plonkup.OffChain.Cardano

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
  → Maybe (GYCredential 'GYKeyRoleStaking)
  → RollupState
  → m (ZKInitializedRollupBuildInfo, GYTxBody)
seedRollup setup mstakeCred initialState = do
  (nft, mintTokenSkel) ← mintTestTokens "zkFold-rollup-nft" 1
  let (nftMP, nftTN) = case nonAdaTokenFromAssetClass nft of
        Nothing → error "seedRollup: absurd, minted token cannot be lovelace"
        Just (GYNonAdaToken nftMP' nftTN') → (nftMP', nftTN')
      rollupScript = zkrbiRollup zkRollupBuildInfo setup nftMP nftTN
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
              { gyTxOutAddress = rollupAddr
              , gyTxOutDatum = Just (datumFromPlutusData initialState, GYTxOutUseInlineDatum)
              , gyTxOutRefS = Nothing
              , gyTxOutValue = valueSingleton nft 1
              }
          )
        <> mintTokenSkel
  let txId = txBodyTxId txBody
      scriptRef = txOutRefFromTuple (txId, 0)
      initializedBuildInfo =
        ZKInitializedRollupBuildInfo
          { zkirbiStakeCred = mstakeCred
          , zkirbiRollupRef = scriptRef
          , zkirbiRollup = rollupScript
          , zkirbiNFT = GYNonAdaToken nftMP nftTN
          }
  pure (initializedBuildInfo, txBody)

-- | Update the rollup state.
updateRollupState ∷ ZKRollupQueryMonad m ⇒ RollupState → ProofBytes → m (GYTxSkeleton 'PlutusV3)
updateRollupState newState proofBytes = do
  rollupAddr ← rollupAddress
  zkirbiRollupRef ← asks zkirbiRollupRef
  zkirbiRollupScript ← asks zkirbiRollup
  zkirbiNFT ← asks zkirbiNFT
  rollupUTxO ←
    utxosAtAddress rollupAddr (Just $ nonAdaTokenToAssetClass zkirbiNFT) >>= \utxos → case utxosToList utxos of
      [utxo] → pure utxo
      _anyOther → throwAppError $ ZKREStateUTxONotFound rollupAddr zkirbiNFT
  let newOut ∷ GYTxOut 'PlutusV3 =
        GYTxOut
          { gyTxOutValue = utxoValue rollupUTxO
          , gyTxOutRefS = utxoRefScript rollupUTxO
          , gyTxOutDatum = Just (datumFromPlutusData newState, GYTxOutUseInlineDatum)
          , gyTxOutAddress = rollupAddr
          }

  pure $
    mustHaveInput
      ( GYTxIn
          { gyTxInTxOutRef = utxoRef rollupUTxO
          , gyTxInWitness =
              GYTxInWitnessScript
                (GYBuildPlutusScriptReference zkirbiRollupRef zkirbiRollupScript)
                Nothing
                (redeemerFromPlutusData proofBytes)
          }
      )
      <> mustHaveOutput newOut