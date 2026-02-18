module ZkFold.Cardano.Rollup.Api (
  rollupAddress,
  seedRollup,
  registerRollupStake,
  updateRollupState,
  bridgeIn,
  bridgeIn',
  utxosAtL2Address,
  byteStringToInteger',
  addressToBS,
) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Reader
import Data.Bifunctor (Bifunctor (..))
import Data.Either (partitionEithers)
import Data.Foldable (Foldable (..))
import Data.Function ((&))
import GeniusYield.Api.TestTokens (mintTestTokens)
import GeniusYield.Examples.Limbo (limboValidatorV2)
import GeniusYield.TxBuilder
import GeniusYield.Types
import GHC.TypeNats (KnownNat)
import ZkFold.Algebra.Class (FromConstant (..), ToConstant (toConstant))
import ZkFold.Cardano.OnChain.Plonkup.Data (ProofBytes)
import ZkFold.Cardano.Rollup.Constants
import ZkFold.Cardano.Rollup.Types
import ZkFold.Cardano.UPLC.RollupSimple.Types (
  BridgeUtxoInfo (..),
  BridgeUtxoStatus (..),
  RollupSimpleRed (..),
  RollupState,
 )
import ZkFold.Cardano.UPLC.RollupSimple.Utils (addressToBS, byteStringToInteger')
import ZkFold.Protocol.Plonkup.OffChain.Cardano
import ZkFold.Data.MerkleTree (Leaves, MerkleTreeSize)
import ZkFold.Data.Vector (fromVector)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Ledger.Types (UTxO (..), Output (..), nullUTxO)
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

bridgeIn ∷ ZKRollupQueryMonad m ⇒ [(GYAddress, GYValue)] → m (GYTxSkeleton 'PlutusV3)
bridgeIn toBridgeIns = do
  bridgeIn' $ first (fromConstant . byteStringToInteger' . addressToBS . addressToPlutus) <$> toBridgeIns

bridgeIn' ∷ ZKRollupQueryMonad m ⇒ [(FieldElement RollupBFInterpreter, GYValue)] → m (GYTxSkeleton 'PlutusV3)
bridgeIn' toBridgeIns = do
  rollupAddr ← rollupAddress
  pure $
    foldMap
      ( \(addr, val) →
          mustHaveOutput
            ( GYTxOut
                { gyTxOutAddress = rollupAddr
                , gyTxOutDatum =
                    Just
                      ( datumFromPlutusData $ BridgeInInitial $ feToInteger addr
                      , GYTxOutUseInlineDatum
                      )
                , gyTxOutRefS = Nothing
                , gyTxOutValue = val
                }
            )
      )
      toBridgeIns

-- | Update the rollup state.
updateRollupState
  ∷ ZKRollupQueryMonad m
  ⇒ RollupState
  → [(GYValue, FieldElement RollupBFInterpreter)]
  -- ^ Value to bridge in along with layer 2 address which will hold this value.
  → [(GYValue, GYAddress)]
  -- ^ Value to bridge-out.
  → ProofBytes
  → m (GYTxSkeleton 'PlutusV3)
updateRollupState newState bridgeIns' bridgeOuts' proofBytes = do
  ZKInitializedRollupBuildInfo {..} ← ask
  rollupAddr ← rollupAddress
  nid ← networkId
  let stakeCred = GYCredentialByScript $ scriptHash zkirbiRollupStake
      stakeAddr = stakeAddressFromCredential nid stakeCred
  si ←
    stakeAddressInfo stakeAddr >>= \case
      Just si → pure si
      Nothing → throwAppError $ ZKREStakeAddressInfoNotFound stakeAddr
  -- For simplicity, we are taking all rollup UTxOs as inputs. This can be made more clever later.
  rollupUTxOs ← utxosAtAddress rollupAddr Nothing
  rollupUTxO ←
    case filterUTxOs (\utxo → valueAssetClass (utxoValue utxo) (nonAdaTokenToAssetClass zkirbiNFT) == 1) rollupUTxOs
      & utxosToList of
      [utxo] → pure utxo
      _anyOther → throwAppError $ ZKREStateUTxONotFound rollupAddr zkirbiNFT

  let rollupUTxOsWithoutState = utxosRemoveTxOutRef (utxoRef rollupUTxO) rollupUTxOs & utxosToList -- sorted by GYTxOutRef.
  gyLogDebug' mempty $ "Rollup UTxOs without state: " <> show rollupUTxOsWithoutState
  eithers ← forM rollupUTxOsWithoutState $ \u → do
    datumTuple ← utxoDatum @_ @BridgeUtxoStatus u
    case datumTuple of
      Right (_, _, BridgeInInitial addr) → pure $ Left (u, addr)
      _ → pure $ Right u

  let (initials, others) = partitionEithers eithers

      -- Match provided bridge-ins against on-chain BridgeInInitial UTxOs by L2 address and value.
      provided = map (\(val, fe) → (feToInteger fe, val)) bridgeIns'
      matchedInitials = map fst $ filter (\(utxo, addr) → (addr, utxoValue utxo) `elem` provided) initials

      -- Only spend matched BridgeInInitial UTxOs + other (non-BridgeInInitial) UTxOs.
      utxosToSpend = matchedInitials <> others

      -- We reverse the given list as in plutus validator, when it traverses the outputs, it adds the later item in list to the beginning of it's own internal list.
      bridgeIns = reverse bridgeIns'
      bridgeOuts = reverse bridgeOuts'
  gyLogDebug' mempty $ "Bridge ins: " <> show bridgeIns
  gyLogDebug' mempty $ "Bridge outs: " <> show bridgeOuts

  forM_ (map fst bridgeIns <> map fst bridgeOuts) $ \value →
    when (fromIntegral (valueTotalAssets value) > zkrsvcMaxOutputAssets zkirbiRollupStakeValConfig) $
      throwAppError $
        ZKREValueHasMoreThanMaxAssets value (fromIntegral (zkrsvcMaxOutputAssets zkirbiRollupStakeValConfig))

  let
    valueOutReq = foldMap' fst bridgeOuts
    -- others are used for bridge-outs and balancing
    valueAvail = foldMap utxoValue others
    valueRem = valueAvail `valueMinus` valueOutReq

  when ((valueOutReq `valueGreater` valueAvail) && valueOutReq /= mempty && (valueOutReq /= valueAvail)) $
    throwAppError $
      ZKREBridgeOutValMoreThanAvail valueAvail valueOutReq

  let
    newOut ∷ GYTxOut 'PlutusV3 =
      GYTxOut
        { gyTxOutValue = utxoValue rollupUTxO
        , gyTxOutRefS = Nothing
        , gyTxOutDatum = Just (datumFromPlutusData newState, GYTxOutUseInlineDatum)
        , gyTxOutAddress = rollupAddr
        }
  -- Note that there are slight issues which may not allow us to support any bridge out case.
  --
  -- 1. Due to minimum ada requirements, since our bridge-out output is having an inline datum, it's minimum ada requirements would be slightly higher. And as onchain validator is strict with respect to amounts, we can only support those bridge-out scenarios where provided ada is sufficient to satisfy minimum ada requirement.
  -- 2. Since minimum ada restriction also apply to bridge balance outputs, it restricts what user can take from available bridged balance as remaining (if any) must satisfy minimum ada requirement.
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
                            { buiStatus = BridgeIn $ feToInteger layer2Addr
                            , buiORef = utxoRef rollupUTxO & txOutRefToPlutusV3
                            }
                      , GYTxOutUseInlineDatum
                      )
                , gyTxOutAddress = rollupAddr
                }
        )
        bridgeIns
      <> foldMap'
        ( \(val, addr) →
            mustHaveOutput $
              GYTxOut
                { gyTxOutValue = val
                , gyTxOutRefS = Nothing
                , gyTxOutDatum =
                    Just
                      ( datumFromPlutusData $
                          BridgeUtxoInfo
                            { buiStatus = BridgeOut
                            , buiORef = utxoRef rollupUTxO & txOutRefToPlutusV3
                            }
                      , GYTxOutUseInlineDatum
                      )
                , gyTxOutAddress = addr
                }
        )
        bridgeOuts
      <> foldMap
        ( \utxo →
            mustHaveInput $
              GYTxIn
                { gyTxInTxOutRef = utxoRef utxo
                , gyTxInWitness = GYTxInWitnessScript (GYBuildPlutusScriptReference zkirbiRollupRef zkirbiRollup) Nothing unitRedeemer
                }
        )
        utxosToSpend
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
      -- For simplicity, we are putting remaining value in a single output. This of course can be problematic if not all assets fit in a single output.
      <> ( if valueRem /= mempty
            then
              mustHaveOutput
                ( GYTxOut
                    { gyTxOutValue = valueRem
                    , gyTxOutRefS = Nothing
                    , gyTxOutDatum =
                        Just
                          ( datumFromPlutusData $
                              BridgeUtxoInfo
                                { buiStatus = BridgeBalance
                                , buiORef = utxoRef rollupUTxO & txOutRefToPlutusV3
                                }
                          , GYTxOutUseInlineDatum
                          )
                    , gyTxOutAddress = rollupAddr
                    }
                )
            else mempty
         )

-- | Query UTxOs at a given L2 address from the UTxO preimage.
utxosAtL2Address
  ∷ (KnownNat a, KnownNat (MerkleTreeSize ud))
  ⇒ FieldElement RollupBFInterpreter
  → Leaves ud (UTxO a RollupBFInterpreter)
  → [UTxO a RollupBFInterpreter]
utxosAtL2Address addr =
  filter (\utxo → oAddress (uOutput utxo) == addr && utxo /= nullUTxO)
    . fromVector

feToInteger ∷ FieldElement RollupBFInterpreter → Integer
feToInteger = fromIntegral @Natural @Integer . toConstant . toConstant