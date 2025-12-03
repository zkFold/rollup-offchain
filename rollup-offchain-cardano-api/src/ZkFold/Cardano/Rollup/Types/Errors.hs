module ZkFold.Cardano.Rollup.Types.Errors (
  ZKRollupException (..),
) where

import Control.Exception (Exception)
import Data.Text qualified as Text
import GeniusYield.HTTP.Errors (GYApiError (..), IsGYApiError (..))
import GeniusYield.Types
import Network.HTTP.Types (status400, status500)

data ZKRollupException
  = ZKREStateUTxONotFound GYAddress GYNonAdaToken
  | ZKREValueHasMoreThanMaxAssets GYValue Natural
  | ZKREStakeAddressInfoNotFound GYStakeAddress
  | ZKREBridgeOutValMoreThanAvail GYValue GYValue
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
  toApiError (ZKREValueHasMoreThanMaxAssets value maxAssets) =
    GYApiError
      { gaeErrorCode = "VALUE_HAS_MORE_THAN_MAX_ASSETS"
      , gaeHttpStatus = status400
      , gaeMsg =
          Text.pack $
            "Value has more than max assets: " <> show value <> " , max assets allowed: " <> show maxAssets <> "."
      }
  toApiError (ZKREStakeAddressInfoNotFound stakeAddr) =
    GYApiError
      { gaeErrorCode = "STAKE_ADDRESS_INFO_NOT_FOUND"
      , gaeHttpStatus = status500
      , gaeMsg =
          Text.pack $
            "Could not find stake address info for the given stake address: " <> show stakeAddr <> "."
      }
  toApiError (ZKREBridgeOutValMoreThanAvail valueAvail valueOutReq) =
    GYApiError
      { gaeErrorCode = "BRIDGE_OUT_VALUE_MORE_THAN_AVAILABLE"
      , gaeHttpStatus = status400
      , gaeMsg =
          Text.pack $
            "Bridge out value, " <> show valueOutReq <> " is more than available value, " <> show valueAvail <> "."
      }
