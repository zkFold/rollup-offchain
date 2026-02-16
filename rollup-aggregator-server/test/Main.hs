module Main (main) where

import GeniusYield.Test.Privnet.Setup (cardanoDefaultTestnetOptionsConway, withPrivnet)
import Test.Tasty (defaultMain, testGroup)

import ZkFold.Cardano.Rollup.Aggregator.Test.EndToEnd (endToEndTests)
import ZkFold.Cardano.Rollup.Aggregator.Test.Validation (validationTests)

main ∷ IO ()
main =
  withPrivnet cardanoDefaultTestnetOptionsConway $ \setup →
    defaultMain $
      testGroup
        "rollup-aggregator-server-tests"
        [ validationTests
        , endToEndTests setup
        ]
