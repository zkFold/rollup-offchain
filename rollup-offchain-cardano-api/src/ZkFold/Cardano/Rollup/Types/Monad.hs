module ZkFold.Cardano.Rollup.Types.Monad (
  ZKRollupQueryMonad,
) where

import Control.Monad.Reader (MonadReader)
import GeniusYield.TxBuilder (GYTxSpecialQueryMonad)
import ZkFold.Cardano.Rollup.Types.Script (ZKInitializedRollupBuildInfo)

type ZKRollupQueryMonad m = (GYTxSpecialQueryMonad m, MonadReader ZKInitializedRollupBuildInfo m)
