module ZkFold.Cardano.Rollup.Aggregator.Options (
  Command (..),
  ServeCommand (..),
  BatchCommand (..),
  parseCommand,
  runCommand,
  runServeCommand,
  runBatchCommand,
) where

import Options.Applicative
import ZkFold.Cardano.Rollup.Aggregator.Run (runBatcher, runServer)

data Command = Serve ServeCommand | Batch BatchCommand

newtype ServeCommand = ServeCommand (Maybe FilePath)

newtype BatchCommand = BatchCommand (Maybe FilePath)

parseCommand ∷ Parser Command
parseCommand =
  subparser $
    mconcat
      [ command
          "serve"
          ( info (Serve <$> parseServeCommand <**> helper) $
              progDesc "Serve HTTP endpoints"
          )
      , command
          "batch"
          ( info (Batch <$> parseBatchCommand <**> helper) $
              progDesc "Run the transaction batcher"
          )
      ]

parseServeCommand ∷ Parser ServeCommand
parseServeCommand =
  ServeCommand
    <$> optional
      ( strOption
          ( long "config"
              <> metavar "CONFIG"
              <> short 'c'
              <> help "Path of optional configuration file. If not provided, \"SERVER_CONFIG\" environment variable is used."
          )
      )

parseBatchCommand ∷ Parser BatchCommand
parseBatchCommand =
  BatchCommand
    <$> optional
      ( strOption
          ( long "config"
              <> metavar "CONFIG"
              <> short 'c'
              <> help "Path of optional configuration file. If not provided, \"SERVER_CONFIG\" environment variable is used."
          )
      )

runCommand ∷ Command → IO ()
runCommand (Serve serveCommand) = runServeCommand serveCommand
runCommand (Batch batchCommand) = runBatchCommand batchCommand

runServeCommand ∷ ServeCommand → IO ()
runServeCommand (ServeCommand mcfp) = runServer mcfp

runBatchCommand ∷ BatchCommand → IO ()
runBatchCommand (BatchCommand mcfp) = runBatcher mcfp
