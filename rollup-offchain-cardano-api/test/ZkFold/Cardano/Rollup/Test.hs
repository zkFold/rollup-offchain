module ZkFold.Cardano.Rollup.Test (rollupTests) where

import GeniusYield.Test.Privnet.Setup
import Test.Tasty (TestTree, testGroup)
import ZkFold.Cardano.Rollup.Test.Update (rollupUpdateTests)

rollupTests ∷ Setup → TestTree
rollupTests setup =
  testGroup
    "privnet-rollup-tests"
    [ rollupUpdateTests setup
    ]
