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
import Data.Int (Int64)
import Data.OpenApi
import Data.OpenApi qualified as OpenApi
import Data.Text qualified as T
import Data.Version (showVersion)
import GHC.Natural (Natural)
import PackageInfo_rollup_aggregator_server qualified as PackageInfo
import Servant
import Servant.OpenApi
import ZkFold.Cardano.Rollup.Aggregator.Auth
import ZkFold.Cardano.Rollup.Aggregator.Orphans ()
import ZkFold.Cardano.Rollup.Aggregator.Types (
  BatchDetailResponse,
  BatchesResponse,
  BridgeInRequest,
  BridgeInResponse,
  BridgeOutsResponse,
  PendingTxsResponse,
  QueryL2UtxosResponse,
  SubmitL1TxRequest,
  SubmitL1TxResponse,
  SubmitTxRequest,
  SubmitTxResponse,
  TxResponse,
  TxsByAddressResponse,
 )
import GeniusYield.Types (GYAddressBech32)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)

-- | Health check endpoint.
type HealthAPI =
  Summary "Health check"
    :> Description
        "Returns 200 OK when the server is operational. \
        \Use this endpoint to verify the service is reachable before submitting transactions."
    :> "health"
    :> Get '[JSON] ()

-- | Submit a single L2 transaction.
type SubmitTxAPI =
  Summary "Submit L2 transaction"
    :> Description
        "Enqueue a signed L2 transaction for batching. \
        \The transaction is validated (bridge-out count, addresses, and values are checked) \
        \then stored with status 'pending' until the batcher picks it up."
    :> "tx"
    :> ReqBody '[JSON] SubmitTxRequest
    :> Post '[JSON] SubmitTxResponse

type BridgeInAPI =
  Summary "Build bridge-in transaction"
    :> Description
        "Construct an unsigned L1 transaction that deposits ADA or native tokens into the rollup \
        \at a given L2 address. The returned transaction must be signed and submitted via \
        \POST /v0/l1/tx/submit."
    :> "bridge"
    :> "in"
    :> ReqBody '[JSON] BridgeInRequest
    :> Post '[JSON] BridgeInResponse

type SubmitL1TxAPI =
  Summary "Submit signed L1 transaction"
    :> Description
        "Attach a witness to a previously built L1 transaction (e.g. a bridge-in) \
        \and submit it to the Cardano network."
    :> "l1"
    :> "tx"
    :> "submit"
    :> ReqBody '[JSON] SubmitL1TxRequest
    :> Post '[JSON] SubmitL1TxResponse

-- | Query UTxOs at a given L2 address.
type QueryL2UtxosAPI =
  Summary "Query L2 UTxOs by address"
    :> Description
        "Return all unspent transaction outputs at the given L2 address \
        \according to the latest persisted ledger state."
    :> "utxos"
    :> QueryParam' '[Required, Strict] "address" (FieldElement RollupBFInterpreter)
    :> Get '[JSON] QueryL2UtxosResponse

-- | Get a single transaction by DB id.
type GetTxAPI =
  Summary "Get transaction by ID"
    :> Description
        "Retrieve a single queued transaction by its database ID, \
        \including its current processing status ('pending', 'processing', or 'batched') \
        \and the batch it was included in, if any."
    :> "tx"
    :> Capture "id" Int64
    :> Get '[JSON] TxResponse

-- | Get all currently pending transactions.
type PendingTxsAPI =
  Summary "List pending transactions"
    :> Description
        "Return all transactions currently waiting to be included in the next batch \
        \(status = 'pending')."
    :> "txs"
    :> "pending"
    :> Get '[JSON] PendingTxsResponse

-- | Get tx history for an L2 address with pagination.
type TxsByAddressAPI =
  Summary "Transaction history for L2 address"
    :> Description
        "Return a paginated list of all transactions that have an output destined for \
        \the given L2 address, ordered newest first. \
        \Defaults: limit = 20, offset = 0."
    :> "txs"
    :> QueryParam' '[Required, Strict] "l2address" (FieldElement RollupBFInterpreter)
    :> QueryParam "limit" Natural
    :> QueryParam "offset" Natural
    :> Get '[JSON] TxsByAddressResponse

-- | Get a single batch by DB id with included transactions.
type GetBatchAPI =
  Summary "Get batch by ID"
    :> Description
        "Retrieve a single batch record by its database ID, \
        \including the L1 transaction hash and the full list of L2 transactions \
        \bundled in that batch."
    :> "batch"
    :> Capture "id" Int64
    :> Get '[JSON] BatchDetailResponse

-- | Get paginated batch list.
type BatchesAPI =
  Summary "List batches"
    :> Description
        "Return a paginated list of submitted batches, ordered newest first. \
        \Each entry includes the L1 transaction hash and the number of L2 transactions bundled. \
        \Defaults: limit = 20, offset = 0."
    :> "batches"
    :> QueryParam "limit" Natural
    :> QueryParam "offset" Natural
    :> Get '[JSON] BatchesResponse

-- | Get bridge-outs (pending + batched) for an L1 address.
type BridgeOutsAPI =
  Summary "List bridge-outs for L1 address"
    :> Description
        "Return all pending and batched bridge-out entries destined for the given L1 bech32 address. \
        \Use this to check which bridge-out outputs are claimable on L1."
    :> "bridge"
    :> "out"
    :> QueryParam' '[Required, Strict] "l1address" GYAddressBech32
    :> Get '[JSON] BridgeOutsResponse

-- | V0 API - combines all endpoints.
type V0API =
  HealthAPI
    :<|> SubmitTxAPI
    :<|> BridgeInAPI
    :<|> SubmitL1TxAPI
    :<|> QueryL2UtxosAPI
    :<|> GetTxAPI
    :<|> PendingTxsAPI
    :<|> TxsByAddressAPI
    :<|> GetBatchAPI
    :<|> BatchesAPI
    :<|> BridgeOutsAPI

-- | Aggregator API with version prefix.
type AggregatorAPI = "v0" :> V0API

-- | Main API including docs.
type MainAPI = AggregatorAPI

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
    & OpenApi.applyTagsFor
      (subOperations (Proxy ∷ Proxy (V0 :> GetTxAPI)) (Proxy ∷ Proxy AggregatorAPI))
      ["Transaction indexing" & OpenApi.description ?~ "Query transaction and batch history."]
    & OpenApi.applyTagsFor
      (subOperations (Proxy ∷ Proxy (V0 :> PendingTxsAPI)) (Proxy ∷ Proxy AggregatorAPI))
      ["Transaction indexing" & OpenApi.description ?~ "Query transaction and batch history."]
    & OpenApi.applyTagsFor
      (subOperations (Proxy ∷ Proxy (V0 :> TxsByAddressAPI)) (Proxy ∷ Proxy AggregatorAPI))
      ["Transaction indexing" & OpenApi.description ?~ "Query transaction and batch history."]
    & OpenApi.applyTagsFor
      (subOperations (Proxy ∷ Proxy (V0 :> GetBatchAPI)) (Proxy ∷ Proxy AggregatorAPI))
      ["Transaction indexing" & OpenApi.description ?~ "Query transaction and batch history."]
    & OpenApi.applyTagsFor
      (subOperations (Proxy ∷ Proxy (V0 :> BatchesAPI)) (Proxy ∷ Proxy AggregatorAPI))
      ["Transaction indexing" & OpenApi.description ?~ "Query transaction and batch history."]
    & OpenApi.applyTagsFor
      (subOperations (Proxy ∷ Proxy (V0 :> BridgeOutsAPI)) (Proxy ∷ Proxy AggregatorAPI))
      ["Bridge" & OpenApi.description ?~ "Bridge operations."]
