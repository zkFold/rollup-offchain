module ZkFold.Cardano.Rollup.Aggregator.Types (
  -- * Fixed Ledger Parameters
  I,
  Ixs,
  Oxs,
  A,
  Bi,
  Bo,
  Ud,
  TxCount,
  Tx,
  TxSignatures,
  QueuedTx (..),

  -- * API Types
  SubmitTxRequest (..),
  SubmitTxResponse (..),
  BridgeInRequest (..),
  BridgeInResponse (..),
  SubmitL1TxRequest (..),
  SubmitL1TxResponse (..),
  QueryL2UtxosResponse (..),

  -- * Indexing Types
  TxStatus (..),
  TxRecord (..),
  BatchRecord (..),
  PendingTxsResponse (..),
  TxResponse (..),
  TxsByAddressResponse (..),
  BatchesResponse (..),
  BatchDetailResponse (..),
  BridgeOutEntry (..),
  BridgeOutsResponse (..),
) where

import Control.Lens ((?~))
import Data.Char (toLower)
import Data.Function ((&))
import Data.Int (Int64)
import Data.OpenApi (NamedSchema (..), ToSchema)
import Data.OpenApi qualified as OpenApi
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Deriving.Aeson
import GHC.Generics ((:*:) (..), (:.:) (..))
import GHC.TypeLits (Symbol)
import GeniusYield.Swagger.Utils (dropSymbolAndCamelToSnake)
import GeniusYield.Types (GYAddress, GYAddressBech32, GYTx, GYTxId, GYTxWitness, GYValue, LowerFirst)
import GeniusYield.Types.OpenApi ()
import ZkFold.Cardano.Rollup.Aggregator.Orphans ()
import ZkFold.Data.Vector (Vector)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Ledger.Types
import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Types.Orphans ()

type I = RollupBFInterpreter

type Ixs = 2

type Oxs = 2

type A = 2

type Bi = 1

type Bo = 1

type Ud = 2

type TxCount = 2

type Tx = Transaction Ixs Oxs A I

type TxSignatures = (Vector Ixs :.: (EdDSAPoint :*: EdDSAScalarField :*: PublicKey)) I

data QueuedTx = QueuedTx
  { qtTransaction ∷ !Tx
  , qtSignatures ∷ !TxSignatures
  , qtBridgeOuts ∷ ![(GYValue, GYAddress)]
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "qt", LowerFirst]] QueuedTx

instance ToSchema QueuedTx where
  declareNamedSchema _ = return $ NamedSchema (Just "QueuedTx") mempty

type SubmitTxReqPrefix ∷ Symbol
type SubmitTxReqPrefix = "str"

data SubmitTxRequest = SubmitTxRequest
  { strTransaction ∷ !Tx
  , strSignatures ∷ !TxSignatures
  , strBridgeOuts ∷ ![(GYValue, GYAddressBech32)]
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix SubmitTxReqPrefix, LowerFirst]] SubmitTxRequest

instance ToSchema SubmitTxRequest where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @SubmitTxReqPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "Request parameters to submit the L2 transaction"

type SubmitTxResPrefix ∷ Symbol
type SubmitTxResPrefix = "str"

newtype SubmitTxResponse = SubmitTxResponse
  { strStatus ∷ Text
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix SubmitTxResPrefix, LowerFirst]] SubmitTxResponse

instance ToSchema SubmitTxResponse where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @SubmitTxResPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "Response to the L2 transaction submission"

type BridgeInReqPrefix ∷ Symbol
type BridgeInReqPrefix = "bir"

data BridgeInRequest = BridgeInRequest
  { birAmount ∷ !GYValue
  , birDestinationAddress ∷ !(FieldElement RollupBFInterpreter)
  , birUsedAddresses ∷ ![GYAddressBech32]
  , birChangeAddress ∷ !GYAddressBech32
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix BridgeInReqPrefix, LowerFirst]] BridgeInRequest

instance ToSchema BridgeInRequest where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @BridgeInReqPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "Request parameters to bridge-in funds"

type BridgeInResPrefix ∷ Symbol
type BridgeInResPrefix = "bir"

newtype BridgeInResponse = BridgeInResponse
  { birTransaction ∷ GYTx
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix BridgeInResPrefix, LowerFirst]] BridgeInResponse

instance ToSchema BridgeInResponse where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @BridgeInResPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "Response containing unsigned bridge-in transaction"

type SubmitL1TxReqPrefix ∷ Symbol
type SubmitL1TxReqPrefix = "sl1tr"

data SubmitL1TxRequest = SubmitL1TxRequest
  { sl1trTransaction ∷ !GYTx
  , sl1trWitness ∷ !GYTxWitness
  }
  deriving stock Generic
  deriving
    FromJSON
    via CustomJSON '[FieldLabelModifier '[StripPrefix SubmitL1TxReqPrefix, LowerFirst]] SubmitL1TxRequest

instance ToSchema SubmitL1TxRequest where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @SubmitL1TxReqPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "Request parameters to submit the L1 transaction"

type SubmitL1TxResPrefix ∷ Symbol
type SubmitL1TxResPrefix = "sl1tr"

newtype SubmitL1TxResponse = SubmitL1TxResponse
  { sl1trTxId ∷ GYTxId
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix SubmitL1TxResPrefix, LowerFirst]] SubmitL1TxResponse

instance ToSchema SubmitL1TxResponse where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @SubmitL1TxResPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "Response to the L1 transaction submission"

type QueryL2UtxosResPrefix ∷ Symbol
type QueryL2UtxosResPrefix = "qlur"

newtype QueryL2UtxosResponse = QueryL2UtxosResponse
  { qlurUtxos ∷ [UTxO A I]
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix QueryL2UtxosResPrefix, LowerFirst]] QueryL2UtxosResponse

instance ToSchema QueryL2UtxosResponse where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @QueryL2UtxosResPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "Response containing UTxOs at the given L2 address"

-- ---------------------------------------------------------------------------
-- Indexing Types
-- ---------------------------------------------------------------------------

data TxStatus = TxPending | TxProcessing | TxBatched
  deriving stock (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[StripPrefix "Tx", LowerFirst]] TxStatus

instance ToSchema TxStatus where
  declareNamedSchema proxy =
    OpenApi.genericDeclareNamedSchema
      OpenApi.defaultSchemaOptions
        { OpenApi.constructorTagModifier = go
        }
      proxy
   where
    go s = case drop 2 s of
      [] → []
      (c : cs) → toLower c : cs

type TxRecordPrefix ∷ Symbol
type TxRecordPrefix = "tr"

data TxRecord = TxRecord
  { trId ∷ !Int64
  , trStatus ∷ !TxStatus
  , trBatchId ∷ !(Maybe Int64)
  , trSubmittedAt ∷ !UTCTime
  , trPayload ∷ !QueuedTx
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix TxRecordPrefix, LowerFirst]] TxRecord

instance ToSchema TxRecord where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @TxRecordPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "A transaction record with status information"

type BatchRecordPrefix ∷ Symbol
type BatchRecordPrefix = "br"

data BatchRecord = BatchRecord
  { brId ∷ !Int64
  , brL1TxId ∷ !Text
  , brCreatedAt ∷ !UTCTime
  , brTxCount ∷ !Int
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix BatchRecordPrefix, LowerFirst]] BatchRecord

instance ToSchema BatchRecord where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @BatchRecordPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "A batch record with L1 transaction info"

type PendingTxsResPrefix ∷ Symbol
type PendingTxsResPrefix = "ptr"

newtype PendingTxsResponse = PendingTxsResponse
  { ptrTxs ∷ [TxRecord]
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix PendingTxsResPrefix, LowerFirst]] PendingTxsResponse

instance ToSchema PendingTxsResponse where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @PendingTxsResPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "Response containing all pending transactions"

type TxResPrefix ∷ Symbol
type TxResPrefix = "txr"

newtype TxResponse = TxResponse
  { txrRecord ∷ TxRecord
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix TxResPrefix, LowerFirst]] TxResponse

instance ToSchema TxResponse where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @TxResPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "Response containing a single transaction"

type TxsByAddressResPrefix ∷ Symbol
type TxsByAddressResPrefix = "tar"

data TxsByAddressResponse = TxsByAddressResponse
  { tarTotal ∷ !Int
  , tarTxs ∷ ![TxRecord]
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix TxsByAddressResPrefix, LowerFirst]] TxsByAddressResponse

instance ToSchema TxsByAddressResponse where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @TxsByAddressResPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "Response containing transactions for an L2 address"

type BatchesResPrefix ∷ Symbol
type BatchesResPrefix = "brsr"

newtype BatchesResponse = BatchesResponse
  { brsrBatches ∷ [BatchRecord]
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix BatchesResPrefix, LowerFirst]] BatchesResponse

instance ToSchema BatchesResponse where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @BatchesResPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "Response containing paginated batch list"

type BatchDetailResPrefix ∷ Symbol
type BatchDetailResPrefix = "bdr"

data BatchDetailResponse = BatchDetailResponse
  { bdrBatch ∷ !BatchRecord
  , bdrTxs ∷ ![TxRecord]
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix BatchDetailResPrefix, LowerFirst]] BatchDetailResponse

instance ToSchema BatchDetailResponse where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @BatchDetailResPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "Response containing batch detail with included transactions"

type BridgeOutEntryPrefix ∷ Symbol
type BridgeOutEntryPrefix = "boe"

data BridgeOutEntry = BridgeOutEntry
  { boeTxId ∷ !Int64
  , boeValue ∷ !GYValue
  , boeStatus ∷ !TxStatus
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix BridgeOutEntryPrefix, LowerFirst]] BridgeOutEntry

instance ToSchema BridgeOutEntry where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @BridgeOutEntryPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "A bridge-out entry with value and status"

type BridgeOutsResPrefix ∷ Symbol
type BridgeOutsResPrefix = "bor"

newtype BridgeOutsResponse = BridgeOutsResponse
  { borEntries ∷ [BridgeOutEntry]
  }
  deriving stock Generic
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix BridgeOutsResPrefix, LowerFirst]] BridgeOutsResponse

instance ToSchema BridgeOutsResponse where
  declareNamedSchema proxy = do
    schema ←
      OpenApi.genericDeclareNamedSchema
        OpenApi.defaultSchemaOptions
          { OpenApi.fieldLabelModifier = dropSymbolAndCamelToSnake @BridgeOutsResPrefix
          }
        proxy
    return $
      schema
        & OpenApi.schema . OpenApi.description ?~ "Response containing bridge-out entries for an L1 address"
