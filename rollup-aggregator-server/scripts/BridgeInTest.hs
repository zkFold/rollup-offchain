-- | Manual bridge-in integration test against a live network (preprod / mainnet).
--
-- This tool:
--   1. Derives the L2 destination address from the operator wallet's L1 address.
--   2. Submits a bridge-in L1 transaction locking the requested amount into the rollup.
--   3. Polls the L2 UTxO endpoint every @--poll-interval@ seconds until the UTxO
--      appears (meaning the batcher fired and ChainSync confirmed), or @--timeout@
--      seconds elapse.
--
-- Prerequisites: the @batch@ process must be running against the same config so
-- that bridge-ins are picked up.  The @serve@ process does NOT need to be running.
--
-- Usage:
--   rollup-bridge-in-test \
--     --config secrets/config-preprod.yaml \
--     --amount 5000000 \
--     [--poll-interval 60] \
--     [--timeout 600]
module Main (main) where

import Control.Concurrent (threadDelay)
import GeniusYield.TxBuilder (runGYTxMonadIO, signAndSubmitConfirmed)
import GeniusYield.Types (GYAddressBech32, GYValue, addressToBech32, valueFromLovelace)
import Options.Applicative
import ZkFold.Cardano.Rollup.Aggregator.Ctx (Ctx (..), runSkeletonI)
import ZkFold.Cardano.Rollup.Aggregator.Handlers (handleConvertAddress, handleQueryL2Utxos)
import ZkFold.Cardano.Rollup.Aggregator.Run (withCtx)
import ZkFold.Cardano.Rollup.Aggregator.Types (
  ConvertAddressRequest (..),
  ConvertAddressResponse (..),
  I,
  QueryL2UtxosResponse (..),
 )
import ZkFold.Cardano.Rollup.Api (bridgeIn)
import ZkFold.Symbolic.Data.FieldElement (FieldElement)

-- * CLI

data Opts = Opts
  { oConfig ∷ !(Maybe FilePath)
  , oAmountLovelace ∷ !Integer
  , oPollIntervalSeconds ∷ !Int
  , oTimeoutSeconds ∷ !Int
  }

parseOpts ∷ Parser Opts
parseOpts =
  Opts
    <$> optional
      ( strOption
          ( long "config"
              <> short 'c'
              <> metavar "FILE"
              <> help "Path to server config YAML (default: SERVER_CONFIG env var)"
          )
      )
    <*> option
      auto
      ( long "amount"
          <> short 'a'
          <> metavar "LOVELACE"
          <> value 5_000_000
          <> showDefault
          <> help "Amount of lovelace to bridge in"
      )
    <*> option
      auto
      ( long "poll-interval"
          <> metavar "SECONDS"
          <> value 60
          <> showDefault
          <> help "How often to check L2 UTxO endpoint"
      )
    <*> option
      auto
      ( long "timeout"
          <> metavar "SECONDS"
          <> value 600
          <> showDefault
          <> help "Give up after this many seconds"
      )

-- * Main

main ∷ IO ()
main = do
  opts ← execParser $ info (parseOpts <**> helper) (fullDesc <> header "Bridge-in integration test")
  withCtx (oConfig opts) $ \_serverConfig ctx → do
    let (sigKey, walletAddr) = ctxSigningKey ctx
        l1Addr ∷ GYAddressBech32 = addressToBech32 walletAddr
        amount ∷ GYValue = valueFromLovelace (oAmountLovelace opts)

    -- 1. Derive L2 address from wallet's L1 address.
    ConvertAddressResponse l2Addr ← handleConvertAddress ctx (ConvertAddressRequest l1Addr)
    putStrLn $ "Wallet L1 address : " <> show l1Addr
    putStrLn $ "L2 destination    : " <> show l2Addr
    putStrLn $ "Bridge-in amount  : " <> show amount

    -- 2. Build tx body, sign, and submit to L1.
    putStrLn "\nBuilding bridge-in transaction..."
    txBody ← runSkeletonI ctx [walletAddr] walletAddr Nothing $
      bridgeIn [(l2Addr, amount)]
    txId ←
      runGYTxMonadIO
        (ctxNetworkId ctx)
        (ctxProviders ctx)
        sigKey
        Nothing
        [walletAddr]
        walletAddr
        Nothing
        $ signAndSubmitConfirmed txBody
    putStrLn $ "Bridge-in submitted: " <> show txId

    -- 3. Poll until the batcher processes the bridge-in and ChainSync confirms.
    let intervalSecs = oPollIntervalSeconds opts
        maxTicks = max 1 (oTimeoutSeconds opts `div` intervalSecs)
    putStrLn $
      "\nWaiting for batcher to process bridge-in (poll every "
        <> show intervalSecs
        <> "s, timeout "
        <> show (maxTicks * intervalSecs)
        <> "s)..."
    pollForUtxo ctx l2Addr intervalSecs maxTicks

-- | Poll @handleQueryL2Utxos@ until a UTxO appears at @l2Addr@ or @maxTicks@ is reached.
pollForUtxo ∷ Ctx → FieldElement I → Int → Int → IO ()
pollForUtxo ctx l2Addr intervalSecs maxTicks = go 0
 where
  go tick = do
    QueryL2UtxosResponse utxos ← handleQueryL2Utxos ctx l2Addr
    let elapsed = tick * intervalSecs
    if not (null utxos)
      then do
        putStrLn $ "\nSuccess! Bridge-in reflected in L2 after ~" <> show elapsed <> "s."
        putStrLn $ "UTxOs at L2 address: " <> show (length utxos)
      else if tick >= maxTicks
        then do
          putStrLn $ "\nTimed out after " <> show elapsed <> "s — UTxO not yet visible."
          putStrLn "Diagnostics:"
          putStrLn "  * Is the batcher running? (rollup-aggregator-server batch --config ...)"
          putStrLn "  * Is ChainSync synced? Check logs for 'ChainSync appears stuck'"
          putStrLn $ "  * sqlite3 <dbPath> \"SELECT * FROM ledger_state; SELECT * FROM batches ORDER BY id DESC LIMIT 3\""
        else do
          putStrLn $ "  [" <> show elapsed <> "s] not yet visible, retrying in " <> show intervalSecs <> "s..."
          threadDelay (intervalSecs * 1_000_000)
          go (tick + 1)
