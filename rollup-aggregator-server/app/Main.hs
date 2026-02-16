module Main (main) where

import Options.Applicative
import ZkFold.Cardano.Rollup.Aggregator.Options

main ∷ IO ()
main = runCommand =<< execParser opts
 where
  opts =
    info
      (parseCommand <**> helper)
      ( fullDesc
          <> progDesc "Transaction Aggregation Server for zkFold Rollup"
          <> header "zkFold Rollup Aggregator Server"
      )
