module ZkFold.Cardano.Rollup.Aggregator.Config (
  -- * Configuration Types
  ServerConfig (..),
  BatchConfig (..),
  ChainSyncStartPoint (..),

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
import Data.Word (Word32, Word64)
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

-- | Optional starting chain point for chain sync.
-- Used only on first run (when no checkpoint is persisted). Set this to a block
-- just before rollup deployment to avoid syncing from genesis.
-- Both the slot number and the block header hash (hex) are required because the
-- Cardano Ouroboros protocol's FindIntersect message needs both.
-- You can obtain the block hash for a given slot from a block explorer or
-- from cardano-cli (e.g. @cardano-cli query tip@).
data ChainSyncStartPoint = ChainSyncStartPoint
  { csspSlot ∷ !Word64
  -- ^ Slot number of the starting block.
  , csspBlockHash ∷ !Text
  -- ^ Block header hash in hex encoding.
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "cssp", LowerFirst]] ChainSyncStartPoint

-- | Batch processing configuration.
data BatchConfig = BatchConfig
  { bcBatchTransactions ∷ !Natural
  -- ^ Exact number of transactions per batch.
  , bcBatchIntervalSeconds ∷ !Natural
  -- ^ How often to create batches (in seconds).
  }
  deriving stock (Eq, Generic, Show)
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
  , scDbPath ∷ !FilePath
  -- ^ SQLite database file path for the transaction queue and ledger state.
  , scNodeSocketPath ∷ !FilePath
  -- ^ Path to the cardano-node socket. Used for chain sync.
  , scChainSyncStartPoint ∷ !(Maybe ChainSyncStartPoint)
  -- ^ Optional starting chain point for first-run chain sync.
  -- When absent, syncing begins from genesis. When present, this slot/hash
  -- is used as the intersection point on first run (no persisted checkpoint).
  -- Has no effect once a checkpoint has been saved.
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
