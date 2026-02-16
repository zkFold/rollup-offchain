module ZkFold.Cardano.Rollup.Aggregator.Options (
  Command (..),
  ServeCommand (..),
  parseCommand,
  runCommand,
  runServeCommand,
) where

import Options.Applicative
import ZkFold.Cardano.Rollup.Aggregator.Run (runServer)

newtype Command = Serve ServeCommand

newtype ServeCommand = ServeCommand (Maybe FilePath)

parseCommand ∷ Parser Command
parseCommand =
  subparser $
    mconcat
      [ command
          "serve"
          ( info (Serve <$> parseServeCommand <**> helper) $
              progDesc "Serve endpoints"
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

runCommand ∷ Command → IO ()
runCommand (Serve serveCommand) = runServeCommand serveCommand

runServeCommand ∷ ServeCommand → IO ()
runServeCommand (ServeCommand mcfp) = runServer mcfp