module ZkFold.Cardano.Rollup.Aggregator.RequestLoggerMiddleware (gcpReqLogger) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Binary.Builder (toLazyByteString)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, parseTimeOrError)
import GeniusYield.Imports (decodeUtf8Lenient, lazyDecodeUtf8Lenient)
import Network.HTTP.Types (statusCode)
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import System.IO (stderr)
import System.Log.FastLogger
import ZkFold.Cardano.Rollup.Aggregator.Utils (bytestringToString)

-- See https://cloud.google.com/logging/docs/structured-logging. This Haskell code defines a middleware for logging HTTP requests in a Google Cloud Platform (GCP) compatible format.
gcpReqLogger ∷ IO Middleware
gcpReqLogger =
  mkRequestLogger
    defaultRequestLoggerSettings
      { outputFormat = CustomOutputFormatWithDetails formatter
      , destination = Handle stderr
      }
 where
  formatter ∷ OutputFormatterWithDetails
  formatter zonedDate req stat _ latency reqBodyChunks resp =
    let statCode = statusCode stat
        method = requestMethod req
        rawLog =
          toLogStr
            . Aeson.encode
            $ Aeson.object
              [ "severity" .= Text.pack (if statCode >= 500 then "ERROR" else "INFO")
              , -- Only log response body for user-error and server-error responses.
                "message" .= if statCode >= 400 then lazyDecodeUtf8Lenient $ toLazyByteString resp else ""
              , "time" .= zonedDateToSensibleTime zonedDate
              , "httpRequest"
                  .= Aeson.object
                    [ "requestMethod" .= bytestringToString method
                    , "requestUrl" .= decodeUtf8Lenient ("https://self" <> rawPathInfo req <> rawQueryString req)
                    , "status" .= statCode
                    , "latency" .= show latency
                    , "reqBody" .= decodeUtf8Lenient (BS.concat reqBodyChunks)
                    ]
              ]
     in rawLog <> "\n" -- Manually adding new line as there doesn't seem to be one in the GCP logs when being monitored through google cloud.

-- Why does wai use ZonedDate from fast-logger + unix-time?
zonedDateToSensibleTime ∷ ByteString → UTCTime
zonedDateToSensibleTime = parseTimeOrError False defaultTimeLocale (bytestringToString simpleTimeFormat) . bytestringToString
