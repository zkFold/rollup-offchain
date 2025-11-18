module ZkFold.Cardano.Rollup.Api (
  rollupAddress,
  updateRollupState,
) where

import Control.Monad.Reader
import GeniusYield.TxBuilder
import GeniusYield.Types
import ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes)
import ZkFold.Cardano.Rollup.Types
import ZkFold.Cardano.UPLC.RollupSimple.Types (RollupState)

-- | Get the rollup address.
rollupAddress ∷ ZKRollupQueryMonad m ⇒ m GYAddress
rollupAddress = do
  zkirbiRollup ← asks zkirbiRollup
  zkirbiStakeCred ← asks zkirbiStakeCred
  nid ← networkId
  pure $ addressFromCredential nid (GYPaymentCredentialByScript $ scriptHash zkirbiRollup) zkirbiStakeCred

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