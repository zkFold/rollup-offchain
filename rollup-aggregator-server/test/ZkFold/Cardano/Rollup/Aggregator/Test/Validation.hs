{-# LANGUAGE TypeApplications #-}

module ZkFold.Cardano.Rollup.Aggregator.Test.Validation (validationTests) where

import Data.ByteString qualified as BS
import GHC.Generics ((:.:) (..))
import GHC.IsList (fromList)
import GHC.Natural (Natural)
import GeniusYield.Types (GYValue, valueFromLovelace, valueFromPlutus)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), Value, adaSymbol, adaToken, singleton)
import PlutusTx.Builtins qualified as PlutusTx
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import ZkFold.Algebra.Class (FromConstant (..), zero)
import ZkFold.Cardano.Rollup.Aggregator.Handlers (matchesBridgeOutValue)
import ZkFold.Cardano.Rollup.Aggregator.Types (I)
import ZkFold.Symbolic.Ledger.Examples.One qualified as Ex1
import ZkFold.Symbolic.Ledger.Types (Output (..), nullOutput)
import ZkFold.Symbolic.Ledger.Types.Value (AssetValue (..), adaName, adaPolicy, nullAssetValue)
import ZkFold.Symbolic.Ledger.Utils (unsafeToVector')

-- * Helpers

-- | Build a 28-byte 'CurrencySymbol' whose big-endian integer encoding equals @n@.
--   Used to construct test native tokens that match symbolic field elements.
mkCurrencySymbol ∷ Integer → CurrencySymbol
mkCurrencySymbol n =
  CurrencySymbol . PlutusTx.toBuiltin $
    BS.replicate 27 0 <> BS.singleton (fromIntegral n)

-- | Build a 'GYValue' from a Plutus 'Value', failing on invalid input.
unsafeValueFromPlutus ∷ Value → GYValue
unsafeValueFromPlutus v = case valueFromPlutus v of
  Right gv → gv
  Left e → error $ "unsafeValueFromPlutus: " <> show e

-- * Type aliases matching Examples.Two parameters

-- | Maximum number of assets per output in Examples.Two.
type A3 = 3

-- * Test CurrencySymbols / TokenNames matching Examples.Two

-- asset2Policy = one + one = 2, asset2Name = adaName = 0
asset2Cs ∷ CurrencySymbol
asset2Cs = mkCurrencySymbol 2

asset2Tn ∷ TokenName
asset2Tn = adaToken -- integer 0, same as adaName

-- asset3Policy = one + one + one = 3, asset3Name = one + one + one = 3
asset3Cs ∷ CurrencySymbol
asset3Cs = mkCurrencySymbol 3

asset3Tn ∷ TokenName
asset3Tn = TokenName (PlutusTx.toBuiltin (BS.singleton 3))

-- * Single-asset (A=1) test outputs

-- | An output with 5M lovelace (mimics bridgeOutOutput from Examples.One).
adaOutput ∷ Output Ex1.A I
adaOutput =
  Output
    { oAddress = fromConstant (19889081452670861588114349990778949346404544631803352712004893411981264611445 ∷ Natural)
    , oAssets =
        Comp1 $
          fromList
            [AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (5_000_000 ∷ Natural)}]
    }

-- | An output with 3M lovelace.
smallAdaOutput ∷ Output Ex1.A I
smallAdaOutput =
  Output
    { oAddress = zero
    , oAssets =
        Comp1 $
          fromList
            [AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (3_000_000 ∷ Natural)}]
    }

-- * Multi-asset (A=3) test outputs (patterns from Examples.Two)

-- | 5 ADA + 25 asset2 + 25 asset3 (like tx1 output 1 in Examples.Two)
--   Asset ordering matches flattenValue(valueToPlutus _) which returns
--   entries in descending CurrencySymbol order: asset3, asset2, ADA.
threeAssetOutput ∷ Output A3 I
threeAssetOutput =
  Output
    { oAddress = zero
    , oAssets =
        Comp1 $
          unsafeToVector'
            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (5_000_000 ∷ Natural)}
            , AssetValue
                { assetPolicy = fromConstant (2 ∷ Natural)
                , assetName = adaName
                , assetQuantity = fromConstant (25_000_000 ∷ Natural)
                }
            , AssetValue
                { assetPolicy = fromConstant (3 ∷ Natural)
                , assetName = fromConstant (3 ∷ Natural)
                , assetQuantity = fromConstant (25_000_000 ∷ Natural)
                }
            ]
    }

-- | 5 ADA + 50 asset2 + null (like tx3 output 1 in Examples.Two: two assets with null padding)
--   flattenValue order: asset2, ADA — then null padding.
twoAssetPaddedOutput ∷ Output A3 I
twoAssetPaddedOutput =
  Output
    { oAddress = zero
    , oAssets =
        Comp1 $
          unsafeToVector'
            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (5_000_000 ∷ Natural)}
            , AssetValue
                { assetPolicy = fromConstant (2 ∷ Natural)
                , assetName = adaName
                , assetQuantity = fromConstant (50_000_000 ∷ Natural)
                }
            , nullAssetValue
            ]
    }

-- | 1 ADA + 25 asset2 + 25 asset3 (like tx2 output 3 in Examples.Two)
--   flattenValue order: asset3, asset2, ADA.
smallThreeAssetOutput ∷ Output A3 I
smallThreeAssetOutput =
  Output
    { oAddress = zero
    , oAssets =
        Comp1 $
          unsafeToVector'
            [ AssetValue {assetPolicy = adaPolicy, assetName = adaName, assetQuantity = fromConstant (1_000_000 ∷ Natural)}
            , AssetValue
                { assetPolicy = fromConstant (2 ∷ Natural)
                , assetName = adaName
                , assetQuantity = fromConstant (25_000_000 ∷ Natural)
                }
            , AssetValue
                { assetPolicy = fromConstant (3 ∷ Natural)
                , assetName = fromConstant (3 ∷ Natural)
                , assetQuantity = fromConstant (25_000_000 ∷ Natural)
                }
            ]
    }

-- | All-null three-slot output.
nullOutput3 ∷ Output A3 I
nullOutput3 = nullOutput @A3 @I

-- * GYValue construction

-- | 5 ADA + 25 asset2 + 25 asset3
threeAssetValue ∷ GYValue
threeAssetValue =
  unsafeValueFromPlutus $
    singleton adaSymbol adaToken 5_000_000
      <> singleton asset2Cs asset2Tn 25_000_000
      <> singleton asset3Cs asset3Tn 25_000_000

-- | 5 ADA + 50 asset2
twoAssetValue ∷ GYValue
twoAssetValue =
  unsafeValueFromPlutus $
    singleton adaSymbol adaToken 5_000_000
      <> singleton asset2Cs asset2Tn 50_000_000

-- | 1 ADA + 25 asset2 + 25 asset3
smallThreeAssetValue ∷ GYValue
smallThreeAssetValue =
  unsafeValueFromPlutus $
    singleton adaSymbol adaToken 1_000_000
      <> singleton asset2Cs asset2Tn 25_000_000
      <> singleton asset3Cs asset3Tn 25_000_000

-- * Tests

validationTests ∷ TestTree
validationTests =
  testGroup
    "Bridge-out value validation"
    [ singleAssetTests
    , multiAssetTests
    ]

singleAssetTests ∷ TestTree
singleAssetTests =
  testGroup
    "Single-asset outputs (A=1)"
    [ testCase "matches correct ADA amount" $
        assertBool "should match" $
          matchesBridgeOutValue adaOutput (valueFromLovelace 5_000_000)
    , testCase "rejects wrong ADA amount" $
        assertBool "should not match" $
          not $
            matchesBridgeOutValue adaOutput (valueFromLovelace 3_000_000)
    , testCase "matches bridgeInOutput from Examples.One" $
        assertBool "should match" $
          matchesBridgeOutValue Ex1.bridgeInOutput (valueFromLovelace 5_000_000)
    , testCase "smaller output matches its own amount" $
        assertBool "should match" $
          matchesBridgeOutValue smallAdaOutput (valueFromLovelace 3_000_000)
    , testCase "smaller output rejects wrong amount" $
        assertBool "should not match" $
          not $
            matchesBridgeOutValue smallAdaOutput (valueFromLovelace 5_000_000)
    , testCase "null output matches empty value" $
        assertBool "should match" $
          matchesBridgeOutValue (nullOutput @Ex1.A @I) mempty
    , testCase "null output rejects non-empty value" $
        assertBool "should not match" $
          not $
            matchesBridgeOutValue (nullOutput @Ex1.A @I) (valueFromLovelace 1_000_000)
    , testCase "non-null output rejects empty value" $
        assertBool "should not match" $
          not $
            matchesBridgeOutValue adaOutput mempty
    ]

multiAssetTests ∷ TestTree
multiAssetTests =
  testGroup
    "Multi-asset outputs (A=3)"
    [ testCase "three distinct assets match" $
        assertBool "should match" $
          matchesBridgeOutValue threeAssetOutput threeAssetValue
    , testCase "three assets reject wrong ADA amount" $
        assertBool "should not match" $
          not $
            matchesBridgeOutValue threeAssetOutput smallThreeAssetValue
    , testCase "two assets with null padding match" $
        assertBool "should match" $
          matchesBridgeOutValue twoAssetPaddedOutput twoAssetValue
    , testCase "two-asset output rejects three-asset value" $
        assertBool "should not match" $
          not $
            matchesBridgeOutValue twoAssetPaddedOutput threeAssetValue
    , testCase "three-asset output rejects two-asset value" $
        assertBool "should not match" $
          not $
            matchesBridgeOutValue threeAssetOutput twoAssetValue
    , testCase "small three-asset output matches its value" $
        assertBool "should match" $
          matchesBridgeOutValue smallThreeAssetOutput smallThreeAssetValue
    , testCase "small three-asset output rejects larger value" $
        assertBool "should not match" $
          not $
            matchesBridgeOutValue smallThreeAssetOutput threeAssetValue
    , testCase "null A=3 output matches empty value" $
        assertBool "should match" $
          matchesBridgeOutValue nullOutput3 mempty
    , testCase "null A=3 output rejects non-empty value" $
        assertBool "should not match" $
          not $
            matchesBridgeOutValue nullOutput3 (valueFromLovelace 1_000_000)
    , testCase "GYValue construction sanity check" $ do
        -- Verify our test GYValues roundtrip correctly
        assertEqual "threeAssetValue should not be empty" False (threeAssetValue == mempty)
        assertEqual "twoAssetValue should not be empty" False (twoAssetValue == mempty)
    ]
