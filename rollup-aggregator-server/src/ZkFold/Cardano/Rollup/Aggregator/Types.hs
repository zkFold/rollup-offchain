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
) where

import Control.Lens ((?~))
import Data.Function ((&))
import Data.OpenApi (ToSchema)
import Data.OpenApi qualified as OpenApi
import Data.Text (Text)
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

data SubmitTxResponse = SubmitTxResponse
  { strStatus ∷ !Text
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
  , birDestinationAddress ∷ !(Either GYAddressBech32 (FieldElement RollupBFInterpreter))
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

data BridgeInResponse = BridgeInResponse
  { birTransaction ∷ !GYTx
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

data SubmitL1TxResponse = SubmitL1TxResponse
  { sl1trTxId ∷ !GYTxId
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
