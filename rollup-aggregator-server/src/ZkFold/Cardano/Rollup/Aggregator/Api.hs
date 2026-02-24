module ZkFold.Cardano.Rollup.Aggregator.Api (
  -- * API Definition
  AggregatorAPI,
  aggregatorAPI,
  aggregatorAPIOpenApi,

  -- * Versioned API
  V0API,
  MainAPI,
  mainAPI,
) where

import Control.Lens ((.~), (?~))
import Data.Function ((&))
import Data.OpenApi
import Data.OpenApi qualified as OpenApi
import Data.Text qualified as T
import Data.Version (showVersion)
import PackageInfo_rollup_aggregator_server qualified as PackageInfo
import Servant
import Servant.OpenApi
import ZkFold.Cardano.Rollup.Aggregator.Auth
import ZkFold.Cardano.Rollup.Aggregator.Orphans ()
import ZkFold.Cardano.Rollup.Aggregator.SwaggerUI (SwaggerUIAPI)
import ZkFold.Cardano.Rollup.Aggregator.Types (
  BridgeInRequest,
  BridgeInResponse,
  QueryL2UtxosResponse,
  SubmitL1TxRequest,
  SubmitL1TxResponse,
  SubmitTxRequest,
  SubmitTxResponse,
 )
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)

-- | Health check endpoint.
type HealthAPI = "health" :> Get '[JSON] ()

-- | Submit a single L2 transaction.
type SubmitTxAPI = "tx" :> ReqBody '[JSON] SubmitTxRequest :> Post '[JSON] SubmitTxResponse

type BridgeInAPI = "bridge" :> "in" :> ReqBody '[JSON] BridgeInRequest :> Post '[JSON] BridgeInResponse

type SubmitL1TxAPI = "l1" :> "tx" :> "submit" :> ReqBody '[JSON] SubmitL1TxRequest :> Post '[JSON] SubmitL1TxResponse

-- | Query UTxOs at a given L2 address.
type QueryL2UtxosAPI =
  "utxos"
    :> QueryParam' '[Required, Strict] "address" (FieldElement RollupBFInterpreter)
    :> Get '[JSON] QueryL2UtxosResponse

-- | V0 API - combines all endpoints.
type V0API =
  HealthAPI
    :<|> SubmitTxAPI
    :<|> BridgeInAPI
    :<|> SubmitL1TxAPI
    :<|> QueryL2UtxosAPI

-- | Serves the OpenAPI JSON specification.
type OpenApiSpecAPI = "openapi.json" :> Get '[JSON] OpenApi

-- | Aggregator API with version prefix.
type AggregatorAPI = "v0" :> V0API

-- | Main API including docs.
type MainAPI = AggregatorAPI :<|> OpenApiSpecAPI :<|> SwaggerUIAPI

-- | Proxy for the aggregator API.
aggregatorAPI ∷ Proxy AggregatorAPI
aggregatorAPI = Proxy

-- | Proxy for the main API.
mainAPI ∷ Proxy MainAPI
mainAPI = Proxy

-- TODO: Name content properly.
aggregatorAPIOpenApi ∷ OpenApi
aggregatorAPIOpenApi =
  toOpenApi aggregatorAPI
    & OpenApi.info
      . OpenApi.title
      .~ "zkFold Rollup Aggregator Server API"
    & OpenApi.info
      . OpenApi.version
      .~ (T.pack . showVersion $ PackageInfo.version)
    & OpenApi.info
      . OpenApi.license
      ?~ ("Apache-2.0" & OpenApi.url ?~ OpenApi.URL "https://opensource.org/licenses/apache-2-0")
    & OpenApi.info
      . OpenApi.contact
      ?~ ( mempty
            & OpenApi.url
              ?~ OpenApi.URL "https://zkfold.io/"
            & OpenApi.email
              ?~ "info@zkfold.io"
            & OpenApi.name
              ?~ "zkFold Technical Support"
         )
    & OpenApi.info
      . OpenApi.description
      ?~ "API to interact with zkFold Rollup Aggregator Server."
    & OpenApi.applyTagsFor
      (subOperations (Proxy ∷ Proxy (V0 :> HealthAPI)) (Proxy ∷ Proxy AggregatorAPI))
      ["Settings" & OpenApi.description ?~ "Endpoint to check if the server is healthy."]
    & OpenApi.applyTagsFor
      (subOperations (Proxy ∷ Proxy (V0 :> SubmitTxAPI)) (Proxy ∷ Proxy AggregatorAPI))
      ["Transactions" & OpenApi.description ?~ "Submit a single L2 transaction for batching."]
    & OpenApi.applyTagsFor
      (subOperations (Proxy ∷ Proxy (V0 :> BridgeInAPI)) (Proxy ∷ Proxy AggregatorAPI))
      ["Bridge" & OpenApi.description ?~ "Bridge operations."]
    & OpenApi.applyTagsFor
      (subOperations (Proxy ∷ Proxy (V0 :> SubmitL1TxAPI)) (Proxy ∷ Proxy AggregatorAPI))
      ["L1 transactions" & OpenApi.description ?~ "Submit L1 transactions."]
    & OpenApi.applyTagsFor
      (subOperations (Proxy ∷ Proxy (V0 :> QueryL2UtxosAPI)) (Proxy ∷ Proxy AggregatorAPI))
      ["L2 utxo queries" & OpenApi.description ?~ "Query L2 UTxO state."]