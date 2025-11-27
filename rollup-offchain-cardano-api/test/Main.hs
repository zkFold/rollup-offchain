module Main (main) where

import GeniusYield.Test.Privnet.Setup
import Test.Tasty (defaultMain, testGroup)
import ZkFold.Cardano.Rollup.Test (rollupTests)

main ∷ IO ()
main = do
  withPrivnet cardanoDefaultTestnetOptionsConway $ \setup →
    defaultMain $
      testGroup
        "zkfold-rollup-tests"
        [ rollupTests setup
        ]
