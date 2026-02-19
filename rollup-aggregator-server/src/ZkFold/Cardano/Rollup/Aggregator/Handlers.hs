module ZkFold.Cardano.Rollup.Aggregator.Handlers (
  -- * Handler Implementation
  aggregatorServer,

  -- * Individual Handlers
  handleHealth,
  handleSubmitTx,
  handleBridgeIn,
  handleSubmitL1Tx,
  handleQueryL2Utxos,

  -- * Validation (exported for testing)
  matchesBridgeOutValue,
) where

import Control.Exception (throwIO)
import Data.Bifunctor (Bifunctor (..))
import GHC.Generics ((:*:) (..), (:.:) (..))
import GHC.TypeNats (KnownNat)
import GeniusYield.Types (
  GYValue,
  addressFromBech32,
  addressToPlutus,
  appendWitnessGYTx,
  assetClassToPlutus,
  gySubmitTx,
  unsignedTx,
  valueToList,
 )
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..))
import PlutusLedgerApi.V1.Value qualified as Plutus
import Servant (ServerError (..), ServerT, err400, (:<|>) (..))
import ZkFold.Algebra.Class (fromConstant, one)
import ZkFold.Cardano.Rollup.Aggregator.Api (AggregatorAPI)
import ZkFold.Cardano.Rollup.Aggregator.Batcher (enqueueTx)
import ZkFold.Cardano.Rollup.Aggregator.Ctx (
  Ctx (..),
  runSkeletonI,
 )
import ZkFold.Cardano.Rollup.Aggregator.Persistence (PersistedState (..), loadState)
import ZkFold.Cardano.Rollup.Aggregator.Types (
  BridgeInRequest (..),
  BridgeInResponse (..),
  I,
  QueryL2UtxosResponse (..),
  QueuedTx (..),
  SubmitL1TxRequest (..),
  SubmitL1TxResponse (..),
  SubmitTxRequest (..),
  SubmitTxResponse (..),
 )
import ZkFold.Cardano.Rollup.Api
import ZkFold.Data.Vector (fromVector)
import ZkFold.Symbolic.Data.Bool (fromBool)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)
import ZkFold.Symbolic.Ledger.Types.Field (RollupBFInterpreter)
import ZkFold.Symbolic.Ledger.Types.Transaction.Core (Output (..), Transaction (..))
import ZkFold.Symbolic.Ledger.Types.Value (AssetValue (..))

-- | Server implementation for the aggregator API.
aggregatorServer ∷ Ctx → ServerT AggregatorAPI IO
aggregatorServer ctx =
  handleHealth ctx
    :<|> handleSubmitTx ctx
    :<|> handleBridgeIn ctx
    :<|> handleSubmitL1Tx ctx
    :<|> handleQueryL2Utxos ctx

-- | Handle health check requests.
handleHealth ∷ Ctx → IO ()
handleHealth _ctx = pure ()

-- | Handle L2 transaction submission.
handleSubmitTx ∷ Ctx → SubmitTxRequest → IO SubmitTxResponse
handleSubmitTx ctx SubmitTxRequest {..} = do
  let outs = fromVector (unComp1 (outputs strTransaction))
      bridgeOuts = filter (\(_ :*: flag) → fromBool flag == one) outs
  if length bridgeOuts /= length strBridgeOuts
    then throwIO $ err400 {errBody = "bridge-out count mismatch"}
    else
      if not $ all validateAddr $ zip bridgeOuts strBridgeOuts
        then throwIO $ err400 {errBody = "bridge-out address mismatch"}
        else
          if not $ all validateValue $ zip bridgeOuts strBridgeOuts
            then throwIO $ err400 {errBody = "bridge-out value mismatch"}
            else do
              let bridgeOutPairs = map (second addressFromBech32) strBridgeOuts
              enqueueTx ctx $ QueuedTx strTransaction strSignatures bridgeOutPairs
              pure $ SubmitTxResponse "queued"
 where
  validateAddr (out :*: _, (_, addrBech32)) =
    let addr = addressFromBech32 addrBech32
        expectedAddrFe = fromConstant $ byteStringToInteger' $ addressToBS $ addressToPlutus addr
     in expectedAddrFe == oAddress out

  validateValue (out :*: _, (val, _)) = matchesBridgeOutValue out val

-- | Check if a 'GYValue' matches the symbolic assets of an 'Output'.
matchesBridgeOutValue ∷ KnownNat a ⇒ Output a I → GYValue → Bool
matchesBridgeOutValue out val =
  let symAssets = fromVector (unComp1 (oAssets out))
      flatVal = valueToList val
      compareAsset av (assetClassToPlutus → Plutus.AssetClass (cs, tn), amt) =
        assetPolicy av == fromConstant (byteStringToInteger' (unCurrencySymbol cs))
          && assetName av == fromConstant (byteStringToInteger' (unTokenName tn))
          && assetQuantity av == fromConstant amt
      isNull av =
        assetPolicy av == fromConstant (0 ∷ Integer)
          && assetName av == fromConstant (0 ∷ Integer)
          && assetQuantity av == fromConstant (0 ∷ Integer)
      (matched, remaining) = splitAt (length flatVal) symAssets
   in length flatVal <= length symAssets
        && and (zipWith compareAsset matched flatVal)
        && all isNull remaining

-- | Handle bridge-in request.
handleBridgeIn ∷ Ctx → BridgeInRequest → IO BridgeInResponse
handleBridgeIn ctx BridgeInRequest {..} = do
  let usedAddrs = map addressFromBech32 birUsedAddresses
      changeAddr = addressFromBech32 birChangeAddress

  txBody ← runSkeletonI ctx usedAddrs changeAddr Nothing $ do
    case birDestinationAddress of
      Left addrBech32 → bridgeIn [(addressFromBech32 addrBech32, birAmount)]
      Right fe → bridgeIn' [(fe, birAmount)]

  let tx = unsignedTx txBody

  pure $ BridgeInResponse tx

-- | Handle L1 transaction submission.
handleSubmitL1Tx ∷ Ctx → SubmitL1TxRequest → IO SubmitL1TxResponse
handleSubmitL1Tx ctx SubmitL1TxRequest {..} = do
  let txWithWitness = appendWitnessGYTx sl1trWitness sl1trTransaction
  txId ← gySubmitTx (ctxProviders ctx) txWithWitness
  pure $ SubmitL1TxResponse txId

-- | Handle L2 UTxO query by address.
handleQueryL2Utxos ∷ Ctx → FieldElement RollupBFInterpreter → IO QueryL2UtxosResponse
handleQueryL2Utxos ctx l2Addr = do
  mState ← loadState (ctxDbPath ctx)
  case mState of
    Nothing → pure $ QueryL2UtxosResponse []
    Just ps → pure $ QueryL2UtxosResponse (utxosAtL2Address l2Addr (psUtxoPreimage ps))
