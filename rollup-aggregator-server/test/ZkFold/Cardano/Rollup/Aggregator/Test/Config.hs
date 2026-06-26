module ZkFold.Cardano.Rollup.Aggregator.Test.Config (configTests) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import ZkFold.Cardano.Rollup.Aggregator.Config (SyncMode (..), resolveSyncModeFields)

configTests ∷ TestTree
configTests =
  testGroup
    "Config"
    [ testCase "defaults to light mode without a node socket" $
        assertEqual "sync mode" SyncLight (resolveSyncModeFields Nothing Nothing)
    , testCase "defaults to node mode when a node socket is configured" $
        assertEqual "sync mode" SyncNode (resolveSyncModeFields Nothing (Just "/tmp/node.socket"))
    , testCase "explicit light mode overrides a configured socket" $
        assertEqual "sync mode" SyncLight (resolveSyncModeFields (Just SyncLight) (Just "/tmp/node.socket"))
    , testCase "decodes sync mode tags" $ do
        assertDecode "\"light\"" SyncLight
        assertDecode "\"node\"" SyncNode
    ]

assertDecode ∷ String → SyncMode → IO ()
assertDecode raw expected =
  case Aeson.eitherDecodeStrict (BS8.pack raw) of
    Left err → assertFailure err
    Right actual → assertEqual raw expected actual
