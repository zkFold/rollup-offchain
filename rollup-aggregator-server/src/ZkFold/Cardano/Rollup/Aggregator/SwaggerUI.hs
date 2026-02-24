module ZkFold.Cardano.Rollup.Aggregator.SwaggerUI (
  HTML,
  SwaggerUIAPI,
  swaggerUIHtml,
) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Network.HTTP.Media ((//), (/:))
import Servant

-- | Minimal HTML content type for Servant.
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
  mimeRender _ = id

-- | Serves the Swagger UI page.
type SwaggerUIAPI = "swagger" :> Get '[HTML] ByteString

-- | HTML page that loads Swagger UI from CDN, pointed at @/openapi.json@.
swaggerUIHtml ∷ ByteString
swaggerUIHtml =
  LBS.unlines
    [ "<!DOCTYPE html>"
    , "<html>"
    , "  <head>"
    , "    <title>zkFold Rollup Aggregator API</title>"
    , "    <meta charset=\"utf-8\"/>"
    , "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
    , "    <link rel=\"stylesheet\" href=\"https://unpkg.com/swagger-ui-dist@5/swagger-ui.css\">"
    , "  </head>"
    , "  <body>"
    , "    <div id=\"swagger-ui\"></div>"
    , "    <script src=\"https://unpkg.com/swagger-ui-dist@5/swagger-ui-bundle.js\"></script>"
    , "    <script src=\"https://unpkg.com/swagger-ui-dist@5/swagger-ui-standalone-preset.js\"></script>"
    , "    <script>"
    , "      window.onload = function() {"
    , "        SwaggerUIBundle({"
    , "          url: '/openapi.json',"
    , "          dom_id: '#swagger-ui',"
    , "          presets: ["
    , "            SwaggerUIBundle.presets.apis,"
    , "            SwaggerUIStandalonePreset"
    , "          ],"
    , "          layout: 'StandaloneLayout'"
    , "        })"
    , "      }"
    , "    </script>"
    , "  </body>"
    , "</html>"
    ]
