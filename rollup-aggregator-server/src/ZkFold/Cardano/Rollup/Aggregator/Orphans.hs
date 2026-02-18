{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.Rollup.Aggregator.Orphans () where

import Control.Lens (at, (%~), (&), (.~), (?~))
import Data.HashMap.Strict.InsOrd qualified as IOHM
import Data.OpenApi
import Data.Text (Text, unpack)
import GHC.Natural (Natural)
import Servant
import Servant.Foreign
import Servant.OpenApi
import Text.Read (readMaybe)
import ZkFold.Algebra.Class (fromConstant)
import ZkFold.Cardano.Rollup.Aggregator.Auth (APIKeyAuthProtect, apiKeyHeaderText)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)

instance ToParamSchema (FieldElement RollupBFInterpreter) where
  toParamSchema _ = mempty & type_ ?~ OpenApiInteger

instance FromHttpApiData (FieldElement RollupBFInterpreter) where
  parseQueryParam t = case readMaybe (unpack t) of
    Nothing → Left $ "Invalid field element (expected non-negative integer): " <> t
    Just n → Right $ fromConstant (n ∷ Natural)

instance HasOpenApi api ⇒ HasOpenApi (APIKeyAuthProtect :> api) where
  toOpenApi _ =
    toOpenApi (Proxy ∷ Proxy api)
      & (components . securitySchemes)
        .~ SecurityDefinitions (IOHM.fromList [(apiKeyHeaderText, apiKeySecurityScheme)])
      & allOperations
        . security
        .~ [SecurityRequirement (IOHM.singleton apiKeyHeaderText [])]
      & allOperations
        . responses
        %~ addCommonResponses
   where
    apiKeySecurityScheme ∷ SecurityScheme
    apiKeySecurityScheme =
      SecurityScheme
        { _securitySchemeType = SecuritySchemeApiKey (ApiKeyParams apiKeyHeaderText ApiKeyHeader)
        , _securitySchemeDescription = Just "API key for accessing the server's API."
        }
    addCommonResponses ∷ Responses → Responses
    addCommonResponses resps = resps & at 401 ?~ Inline response401 & at 403 ?~ Inline response403 & at 500 ?~ Inline response500

    response401 ∷ Response
    response401 = mempty & description .~ "Unauthorized access - API key missing"

    response403 ∷ Response
    response403 = mempty & description .~ "Forbidden - The API key does not have permission to perform the request"

    response500 ∷ Response
    response500 = mempty & description .~ "Internal server error"

-- `HasForeign` instance for `APIKeyAuthProtect :> api` is required to generate client code using libraries such as `servant-py`.
-- This is written with help from https://github.com/haskell-servant/servant-auth/issues/8#issue-185541839.
instance
  ∀ lang ftype api
   . (HasForeign lang ftype api, HasForeignType lang ftype Text)
  ⇒ HasForeign lang ftype (APIKeyAuthProtect :> api)
  where
  type Foreign ftype (APIKeyAuthProtect :> api) = Foreign ftype api
  foreignFor lang Proxy Proxy subR = foreignFor lang Proxy (Proxy ∷ Proxy api) subR'
   where
    subR' = subR {_reqHeaders = HeaderArg arg : _reqHeaders subR}
    arg =
      Arg
        { _argName = "api-key"
        , _argType = typeFor lang (Proxy ∷ Proxy ftype) (Proxy ∷ Proxy Text)
        }