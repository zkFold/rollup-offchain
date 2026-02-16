module ZkFold.Cardano.Rollup.Aggregator.Run (
  runServer,
) where

import Control.Exception (Exception (..), SomeException (..), throwIO, try)
import Control.Monad.Except (ExceptT (..))
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Text.Lazy qualified as LT
import Data.Yaml.Pretty qualified as Yaml
import GeniusYield.GYConfig (Confidential (..), withCfgProviders)
import GeniusYield.HTTP.Errors (someBackendError)
import GeniusYield.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors qualified as Cors
import Servant
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Server.Internal.ServerError (responseServerError)
import System.TimeManager (TimeoutThread (..))
import ZkFold.Cardano.Rollup.Aggregator.Api
import ZkFold.Cardano.Rollup.Aggregator.Auth
import ZkFold.Cardano.Rollup.Aggregator.Batcher (initBatcherState, startBatcher)
import ZkFold.Cardano.Rollup.Aggregator.Persistence (loadState)
import ZkFold.Cardano.Rollup.Aggregator.Config
import ZkFold.Cardano.Rollup.Aggregator.Ctx
import ZkFold.Cardano.Rollup.Aggregator.ErrorMiddleware
import ZkFold.Cardano.Rollup.Aggregator.Handlers (aggregatorServer)
import ZkFold.Cardano.Rollup.Aggregator.RequestLoggerMiddleware (gcpReqLogger)
import ZkFold.Cardano.Rollup.Aggregator.Utils
import ZkFold.Cardano.Rollup.Constants (zkRollupBuildInfo)
import ZkFold.Cardano.Rollup.Types (
  ZKInitializedRollupBuildInfo (..),
  ZKRollupBuildInfo (..),
  ZKRollupStakeValConfig (..),
 )

-- | Run the server by loading configuration from a file.
runServer ∷ Maybe FilePath → IO ()
runServer mConfigPath = do
  serverConfig ← serverConfigOptionalFPIO mConfigPath
  signingKey ← signingKeyFromServerConfig serverConfig
  let nid = scNetworkId serverConfig
      coreCfg = coreConfigFromServerConfig serverConfig
  withCfgProviders coreCfg "aggregator-server" $ \providers → do
    let logInfoS = gyLogInfo providers mempty
        logErrorS = gyLogError providers mempty
    logInfoS $
      "\nServer configuration: "
        <> "\nPort: "
        <> show (scPort serverConfig)
        <> "\nAddress of wallet: "
        <> show (snd signingKey)
        <> "\nCollateral: "
        <> show (scCollateral serverConfig)
        <> "\nRollup NFT: "
        <> show (scRollupNFT serverConfig)
        <> "\nRollup Address: "
        <> show (scRollupAddr serverConfig)
        <> "\nRollup Script Ref: "
        <> show (scRollupScriptRef serverConfig)
        <> "\nRollup Stake Script Ref: "
        <> show (scRollupStakeScriptRef serverConfig)
        <> "\nMax Bridge In: "
        <> show (scMaxBridgeIn serverConfig)
        <> "\nMax Bridge Out: "
        <> show (scMaxBridgeOut serverConfig)
        <> "\nMax Output Assets: "
        <> show (scMaxOutputAssets serverConfig)
        <> "\nBatch Config: "
        <> show (scBatchConfig serverConfig)
        <> "\nCore Config: "
        <> show coreCfg
    BS.writeFile "web/openapi/api.yaml" (Yaml.encodePretty Yaml.defConfig aggregatorAPIOpenApi)
    reqLoggerMiddleware ← gcpReqLogger

    nftToken ← case scRollupNFT serverConfig of
      GYToken pid tn → pure $ GYNonAdaToken pid tn
      GYLovelace → throwIO $ userError "Rollup NFT cannot be Ada"

    let stakeValConfig =
          ZKRollupStakeValConfig
            { zkrsvcNFT = nftToken
            , zkrsvcSetupBytes = scSetupBytes serverConfig
            , zkrsvcMaxBridgeIn = scMaxBridgeIn serverConfig
            , zkrsvcMaxBridgeOut = scMaxBridgeOut serverConfig
            , zkrsvcMaxOutputAssets = scMaxOutputAssets serverConfig
            }

    let rollupStakeScript = zkrbiRollupStake zkRollupBuildInfo stakeValConfig
        rollupStakeScriptHash = scriptHash rollupStakeScript
        rollupScript = zkrbiRollup zkRollupBuildInfo rollupStakeScriptHash

    let buildInfo =
          ZKInitializedRollupBuildInfo
            { zkirbiRollup = rollupScript
            , zkirbiRollupStake = rollupStakeScript
            , zkirbiNFT = nftToken
            , zkirbiStakeCred = addressToStakeCredential $ addressFromBech32 (scRollupAddr serverConfig)
            , zkirbiRollupRef = scRollupScriptRef serverConfig
            , zkirbiRollupStakeRef = scRollupStakeScriptRef serverConfig
            , zkirbiRollupStakeValConfig = stakeValConfig
            }

    mPersistedState ← case scStatePersistPath serverConfig of
      Just path → do
        mState ← loadState path
        case mState of
          Just _ → logInfoS "Restored persisted ledger state from disk."
          Nothing → logInfoS "No persisted state found, starting fresh."
        pure mState
      Nothing → do
        logInfoS "State persistence not configured, starting fresh."
        pure Nothing
    (batchQueue, ledgerStateVar, utxoPreimageVar, trustedSetup, ledgerCircuit, proverSecret) ← initBatcherState mPersistedState
    let
      -- These are only meant to catch fatal exceptions, application thrown exceptions should be caught beforehand.
      onException ∷ req → SomeException → IO ()
      onException _req exc =
        displayException exc
          & if isMatchedException exceptionsToIgnore exc
            then logInfoS
            else logErrorS
       where
        -- TimeoutThread and Warp.ConnectionClosedByPeer do not indicate that anything is wrong and
        -- should not be logged as errors. See
        -- https://magnus.therning.org/2021-07-03-the-timeout-manager-exception.html
        -- https://www.rfc-editor.org/rfc/rfc5246#page-29
        exceptionsToIgnore = Proxy @TimeoutThread :>> Proxy @Warp.InvalidRequest :>> ENil
      onExceptionResponse ∷ SomeException → Wai.Response
      onExceptionResponse _ = responseServerError . apiErrorToServerError $ someBackendError "Internal Server Error"
      corsPolicy =
        Cors.simpleCorsResourcePolicy
          { Cors.corsRequestHeaders = ["Content-Type", "api-key"]
          , Cors.corsMethods = ["GET", "POST", "PUT", "OPTIONS"]
          , Cors.corsOrigins = Nothing -- Allow all origins, restrict as needed
          }
      settings =
        Warp.defaultSettings
          & Warp.setPort (scPort serverConfig)
          & Warp.setOnException onException
          & Warp.setOnExceptionResponse onExceptionResponse
      errLoggerMiddleware = errorLoggerMiddleware $ logErrorS . LT.unpack
      ctx =
        Ctx
          { ctxNetworkId = nid
          , ctxProviders = providers
          , ctxSigningKey = signingKey
          , ctxCollateral = scCollateral serverConfig
          , ctxRollupBuildInfo = buildInfo
          , ctxBatchConfig = scBatchConfig serverConfig
          , ctxBatchQueue = batchQueue
          , ctxLedgerStateVar = ledgerStateVar
          , ctxUtxoPreimageVar = utxoPreimageVar
          , ctxTrustedSetup = trustedSetup
          , ctxLedgerCircuit = ledgerCircuit
          , ctxProverSecret = proverSecret
          , ctxStatePersistPath = scStatePersistPath serverConfig
          }

    -- TODO: We should make batcher separate & fault-tolerant to the server.
    startBatcher ctx
    Warp.runSettings settings
      . reqLoggerMiddleware
      . errLoggerMiddleware
      . errorJsonWrapMiddleware
      . Cors.cors (const $ Just corsPolicy)
      $ let context = apiKeyAuthHandler (case scApiKey serverConfig of Confidential t → apiKeyFromText t) :. EmptyContext
         in serveWithContext mainAPI context
              $ hoistServerWithContext
                mainAPI
                (Proxy ∷ Proxy '[AuthHandler Wai.Request ()])
                (\ioAct → Handler . ExceptT $ first (apiErrorToServerError . exceptionHandler) <$> try ioAct)
              $ aggregatorServer ctx
