module ZkFold.Cardano.Rollup.Types.Errors (
  ZKRollupException (..),
) where

import Control.Exception (Exception)
import Data.Text qualified as Text
import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (..))
import GeniusYield.Types
import Network.HTTP.Types (status500)

data ZKRollupException
  = ZKREStateUTxONotFound GYAddress GYNonAdaToken
  deriving stock Show
  deriving anyclass Exception

instance IsGYApiError ZKRollupException where
  toApiError (ZKREStateUTxONotFound rollupAddr nft) =
    GYApiError
      { gaeErrorCode = "STATE_UTXO_NOT_FOUND"
      , gaeHttpStatus = status500
      , gaeMsg =
          Text.pack $
            "Could not find state UTXO for the given rollup address: " <> show rollupAddr <> " and NFT: " <> show nft <> "."
      }