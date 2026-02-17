module ZkFold.Cardano.Rollup.Aggregator.Config (
  -- * Configuration Types
  ServerConfig (..),
  BatchConfig (..),

  -- * Configuration Loading
  serverConfigOptionalFPIO,
  signingKeyFromServerConfig,
  coreConfigFromServerConfig,
) where

import Control.Exception (throwIO)
import Data.Aeson (eitherDecodeFileStrict, eitherDecodeStrict)
import Data.Bifunctor (Bifunctor (..))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Word (Word32)
import Data.Yaml qualified as Yaml
import Deriving.Aeson
import GeniusYield.GYConfig (Confidential, GYCoreConfig (..), GYCoreProviderInfo)
import GeniusYield.Types hiding (Port)
import Network.Wai.Handler.Warp (Port)
import System.Envy (FromEnv (..), decodeEnv, env)
import System.FilePath.Posix (takeExtension)
import ZkFold.Protocol.Plonkup.OffChain.Cardano (ZKSetupBytes)

-- | User wallet configuration.
data UserWallet = MnemonicWallet !MnemonicWalletDetails | KeyPathWallet !FilePath
  deriving stock Generic
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[LowerFirst]] UserWallet

-- | Mnemonic wallet details.
data MnemonicWalletDetails = MnemonicWalletDetails
  { mnemonic ∷ !Mnemonic
  -- ^ Mnemonic (seed phrase).
  , accIx ∷ !(Maybe Word32)
  -- ^ Account index.
  , addrIx ∷ !(Maybe Word32)
  -- ^ Payment address index.
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

-- | Batch processing configuration.
data BatchConfig = BatchConfig
  { bcBatchTransactions ∷ !Natural
  -- ^ Exact number of transactions per batch.
  , bcBatchIntervalSeconds ∷ !Natural
  -- ^ How often to create batches (in seconds).
  }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "bc", LowerFirst]] BatchConfig

-- | Server configuration.
data ServerConfig = ServerConfig
  { scCoreProvider ∷ !GYCoreProviderInfo
  -- ^ Cardano provider information.
  , scNetworkId ∷ !GYNetworkId
  -- ^ Cardano network ID.
  , scLogging ∷ ![GYLogScribeConfig]
  -- ^ Logging configuration.
  , scPort ∷ !Port
  -- ^ Server port.
  , scWallet ∷ !UserWallet
  -- ^ Operator (server's) wallet.
  , scCollateral ∷ !GYTxOutRef
  -- ^ Collateral UTxO.
  , scRollupNFT ∷ !GYAssetClass
  -- ^ Rollup state NFT.
  , scRollupAddr ∷ !GYAddressBech32
  -- ^ Rollup address.
  , scRollupScriptRef ∷ !GYTxOutRef
  -- ^ Reference to the rollup spending script.
  , scRollupStakeScriptRef ∷ !GYTxOutRef
  -- ^ Reference to the rollup stake script.
  , scSetupBytes ∷ !ZKSetupBytes
  -- ^ Setup bytes.
  , scMaxBridgeIn ∷ !Natural
  -- ^ Maximum bridge in.
  , scMaxBridgeOut ∷ !Natural
  -- ^ Maximum bridge out.
  , scMaxOutputAssets ∷ !Natural
  -- ^ Maximum output assets.
  , scBatchConfig ∷ !BatchConfig
  -- ^ Batch configuration.
  , scApiKey ∷ !(Confidential Text)
  -- ^ API key.
  , scStatePersistPath ∷ !FilePath
  -- ^ File path for persisting ledger state across restarts.
  }
  deriving stock Generic
  deriving
    FromJSON
    via CustomJSON '[FieldLabelModifier '[StripPrefix "sc", LowerFirst]] ServerConfig

instance FromEnv ServerConfig where
  fromEnv _ = forceFromJsonOrYaml <$> env "SERVER_CONFIG"
   where
    forceFromJsonOrYaml ∷ FromJSON a ⇒ String → a
    forceFromJsonOrYaml s =
      let bs = fromString s
          parseResults = eitherDecodeStrict bs :| [first show $ Yaml.decodeEither' bs]
       in go parseResults
     where
      go (x :| []) = case x of
        Left e → error e
        Right a → a
      go (x :| y : ys) = case x of
        Left _ → go (y :| ys)
        Right a → a

eitherDecodeFileStrictJsonOrYaml ∷ FromJSON a ⇒ FilePath → IO (Either String a)
eitherDecodeFileStrictJsonOrYaml fp =
  case takeExtension fp of
    ".json" → eitherDecodeFileStrict fp
    ".yaml" → first show <$> Yaml.decodeFileEither fp
    _ → throwIO $ userError "Only .json or .yaml extensions are supported for configuration."

serverConfigOptionalFPIO ∷ Maybe FilePath → IO ServerConfig
serverConfigOptionalFPIO mfp = do
  e ← maybe decodeEnv eitherDecodeFileStrictJsonOrYaml mfp
  either (throwIO . userError) return e

signingKeyFromServerConfig ∷ ServerConfig → IO (GYSomePaymentSigningKey, GYAddress)
signingKeyFromServerConfig ServerConfig {..} = do
  case scWallet of
    (MnemonicWallet MnemonicWalletDetails {..}) →
      let wk' = walletKeysFromMnemonicIndexed mnemonic (fromMaybe 0 accIx) (fromMaybe 0 addrIx)
       in case wk' of
            Left e → throwIO $ userError $ "Failed to get wallet keys from mnemonic: " <> show e
            Right wk → pure (AGYExtendedPaymentSigningKey (walletKeysToExtendedPaymentSigningKey wk), walletKeysToAddress wk scNetworkId)
    (KeyPathWallet fp) → do
      skey ← readSomePaymentSigningKey fp
      pure (skey, addressFromSomePaymentSigningKey scNetworkId skey)
 where
  addressFromSomePaymentSigningKey ∷ GYNetworkId → GYSomePaymentSigningKey → GYAddress
  addressFromSomePaymentSigningKey nid skey =
    let pkh =
          case skey of
            AGYPaymentSigningKey skey' → paymentKeyHash . paymentVerificationKey $ skey'
            AGYExtendedPaymentSigningKey skey' →
              getExtendedVerificationKey skey'
                & extendedVerificationKeyHash
     in addressFromPaymentKeyHash nid pkh

coreConfigFromServerConfig ∷ ServerConfig → GYCoreConfig
coreConfigFromServerConfig ServerConfig {..} =
  GYCoreConfig
    { cfgCoreProvider = scCoreProvider
    , cfgNetworkId = scNetworkId
    , cfgLogging = scLogging
    , cfgLogTiming = Nothing
    }