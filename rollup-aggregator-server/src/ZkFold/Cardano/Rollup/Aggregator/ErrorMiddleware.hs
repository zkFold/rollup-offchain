module ZkFold.Cardano.Rollup.Aggregator.ErrorMiddleware (
  exceptionHandler,
  errorJsonWrapMiddleware,
  errorLoggerMiddleware,
  apiErrorToServerError,
) where

import Control.Exception (Exception (..), SomeException)
import Control.Monad (when)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (toUpper)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import GeniusYield.HTTP.Errors
import GeniusYield.Imports (decodeUtf8Lenient, encodeUtf8, lazyDecodeUtf8Lenient, (&))
import Network.HTTP.Types (
  Status (statusCode, statusMessage),
  mkStatus,
 )
import Network.Wai qualified as Wai
import Servant.Server (ServerError (..))

-- | This is used for turning non-json responses into JSON.
--
-- Example of responses which are not in JSON: Servant body parse error, url not found error etc.
errorJsonWrapMiddleware ∷ Wai.Middleware
errorJsonWrapMiddleware app req respond = app req $ \res → do
  let (status, headers, body) = Wai.responseToStream res
  if lookup "Content-Type" headers
    /= Just "application/json" -- Don't overwrite responses which are already json!
    && statusCode status
      >= 400
    && statusCode status
      < 600
    then do
      lbs ←
        if statusCode status == 404
          then -- The body in a 404 Servant err is empty for some reason.
            pure . LBS.fromStrict $ "Not Found"
          else sinkStreamingBody body
      respond $ errorResponse status lbs
    else respond res

errorLoggerMiddleware ∷ (LT.Text → IO ()) → Wai.Middleware
errorLoggerMiddleware errorLogger app req respond = app req $ \res → do
  let (status, _headers, body) = Wai.responseToStream res
  when (statusCode status >= 400 && statusCode status < 600) $
    sinkStreamingBody body
      >>= errorLogger
        . lazyDecodeUtf8Lenient
  respond res

-- TODO: Handle all known exceptions in exceptionHandler.

-- | Reinterpret exceptions raised by the server (mostly contract exceptions) into 'GYApiError's.
--
-- Use 'apiErrorToServerError' to construct a server response out of 'GYApiError'.
exceptionHandler ∷ SomeException → GYApiError
exceptionHandler =
  catchesWaiExc
    [ WH $ \case
        ServerError {..} →
          GYApiError
            { gaeErrorCode = "SERVER_ERROR"
            , gaeHttpStatus = mkStatus errHTTPCode (errReasonPhrase & T.pack & encodeUtf8)
            , gaeMsg = decodeUtf8Lenient (LBS.toStrict errBody)
            }
    ]

sinkStreamingBody ∷ ((Wai.StreamingBody → IO ()) → IO ()) → IO LBS.ByteString
sinkStreamingBody k = do
  ref ← newIORef mempty
  k $ \f → f (\b → modifyIORef' ref (<> b)) (return ())
  toLazyByteString <$> readIORef ref

errorResponse ∷ Status → LBS.ByteString → Wai.Response
errorResponse status body =
  Wai.responseLBS
    status
    [("Content-Type", "application/json")]
    $ Aeson.encode
    $ Aeson.object
      [ "errorCode" Aeson..= bsMsgToCode (statusMessage status)
      , "message" Aeson..= decodeUtf8Lenient (LBS.toStrict body)
      ]
 where
  bsMsgToCode = T.map (\case ' ' → '_'; x → toUpper x) . decodeUtf8Lenient

-- | Transform a 'GYApiError' to 'ServerError'.
apiErrorToServerError ∷ GYApiError → ServerError
apiErrorToServerError GYApiError {gaeHttpStatus, gaeErrorCode, gaeMsg} =
  ServerError
    { errHTTPCode = statusCode gaeHttpStatus
    , errReasonPhrase = T.unpack . decodeUtf8Lenient $ statusMessage gaeHttpStatus
    , errBody = Aeson.encode $ Aeson.object ["errorCode" .= gaeErrorCode, "message" .= gaeMsg]
    , errHeaders = [("Content-Type", "application/json")]
    }

data WaiExceptionHandler = ∀ e. Exception e ⇒ WH (e → GYApiError)

catchesWaiExc ∷ [WaiExceptionHandler] → SomeException → GYApiError
catchesWaiExc handlers e = foldr tryHandler (someBackendError $ displayException' e) handlers
 where
  tryHandler (WH handler) res = maybe res handler $ fromException e

displayException' ∷ Exception e ⇒ e → Text
displayException' = T.pack . displayException