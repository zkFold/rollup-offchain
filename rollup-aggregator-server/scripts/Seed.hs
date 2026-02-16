module Main (main) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson (object, (.=))
import Data.ByteString hiding (reverse)
import Data.Function ((&))
import Data.Yaml qualified as Yaml
import GeniusYield.Debug (coreConfigIO)
import GeniusYield.GYConfig (GYCoreConfig (..), withCfgProviders)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import ZkFold.Cardano.Rollup.Aggregator.Batcher (initialState)
import ZkFold.Cardano.Rollup.Aggregator.Persistence (saveState)
import ZkFold.Cardano.Rollup.Aggregator.Types
import ZkFold.Symbolic.Ledger.Types (nullUTxO)
import ZkFold.Cardano.Rollup.Api
import ZkFold.Cardano.Rollup.Api.Utils (stateToRollupState)
import ZkFold.Cardano.Rollup.Types
import ZkFold.Protocol.NonInteractiveProof (powersOfTauSubset)
import ZkFold.Symbolic.Ledger.Circuit.Compile (
  ledgerCircuit,
  ledgerSetup,
  mkSetup,
 )

main ∷ IO ()
main = runCommand =<< execParser opts
 where
  opts =
    info
      (parseRollupSeedCommand <**> helper)
      ( fullDesc
          <> header "Seed zkFold Rollup"
      )

data RollupSeedCommand = RollupSeedCommand
  { rscProviderConfig ∷ !FilePath
  , rscSigningKey ∷ !FilePath
  , rscOutput ∷ !FilePath
  , rscStateFile ∷ !FilePath
  , rscMaxBridgeIn ∷ !Natural
  , rscMaxBridgeOut ∷ !Natural
  , rscMaxOutputAssets ∷ !Natural
  }

parseRollupSeedCommand ∷ Parser RollupSeedCommand
parseRollupSeedCommand =
  RollupSeedCommand
    <$> strOption
      ( long "config"
          <> metavar "CONFIG"
          <> short 'c'
          <> help "Path of Atlas provider configuration file."
      )
    <*> strOption
      ( long "signing-key"
          <> metavar "SIGNING_KEY"
          <> short 's'
          <> help "Path of signing key file."
      )
    <*> strOption
      ( long "output"
          <> metavar "OUTPUT"
          <> short 'o'
          <> help "Path of output YAML file."
      )
    <*> strOption
      ( long "state-file"
          <> metavar "STATE_FILE"
          <> help "Path of initial state JSON file for server persistence."
      )
    <*> option
      auto
      ( long "max-bridge-in"
          <> metavar "MAX_BRIDGE_IN"
          <> value 1
          <> showDefault
          <> help "Max bridge in outputs."
      )
    <*> option
      auto
      ( long "max-bridge-out"
          <> metavar "MAX_BRIDGE_OUT"
          <> value 1
          <> showDefault
          <> help "Max bridge out outputs."
      )
    <*> option
      auto
      ( long "max-output-assets"
          <> metavar "MAX_OUTPUT_ASSETS"
          <> value 1
          <> showDefault
          <> help "Max output assets."
      )

runCommand ∷ RollupSeedCommand → IO ()
runCommand RollupSeedCommand {..} = do
  coreConfig ← coreConfigIO rscProviderConfig
  signingKey ∷ GYSigningKey 'GYKeyRolePayment ← readSigningKey rscSigningKey
  let nid = cfgNetworkId coreConfig
      signingKeyAddress = signingKey & getVerificationKey & verificationKeyHash & addressFromPaymentKeyHash nid
      state0 = initialState
      rollupState0 = stateToRollupState state0

  Prelude.putStrLn "Generating setup parameters (this may take a while)..."
  ts ← powersOfTauSubset
  let compiledCircuit = ledgerCircuit @Bi @Bo @Ud @A @Ixs @Oxs @TxCount @I
  let setupB = ledgerSetup @ByteString @Bi @Bo @Ud @A @Ixs @Oxs @TxCount @I ts compiledCircuit & mkSetup

  withCfgProviders coreConfig "rollup-seed" $ \providers → do
    (initializedBuildInfo, txBodySeed) ←
      runGYTxMonadIO nid providers (AGYPaymentSigningKey signingKey) Nothing [signingKeyAddress] signingKeyAddress Nothing $
        seedRollup setupB rscMaxBridgeIn rscMaxBridgeOut rscMaxOutputAssets Nothing rollupState0

    tidSeed ←
      runGYTxMonadIO nid providers (AGYPaymentSigningKey signingKey) Nothing [signingKeyAddress] signingKeyAddress Nothing $
        signAndSubmitConfirmed txBodySeed
    Prelude.putStrLn $ "Seed rollup transaction submitted: " <> show tidSeed

    txBodyRegisterStake ←
      runGYTxMonadIO nid providers (AGYPaymentSigningKey signingKey) Nothing [signingKeyAddress] signingKeyAddress Nothing $
        runReaderT (registerRollupStake >>= buildTxBody) initializedBuildInfo

    tidRegisterStake ←
      runGYTxMonadIO nid providers (AGYPaymentSigningKey signingKey) Nothing [signingKeyAddress] signingKeyAddress Nothing $
        signAndSubmitConfirmed txBodyRegisterStake
    Prelude.putStrLn $ "Register stake transaction submitted: " <> show tidRegisterStake

    writeRollupConfig nid rscOutput initializedBuildInfo

    let initialUtxoPreimage = pure (nullUTxO @A @I)
    createDirectoryIfMissing True (takeDirectory rscStateFile)
    saveState rscStateFile state0 initialUtxoPreimage
    Prelude.putStrLn $ "Initial state written to: " <> rscStateFile

writeRollupConfig ∷ GYNetworkId → FilePath → ZKInitializedRollupBuildInfo → IO ()
writeRollupConfig nid fp ZKInitializedRollupBuildInfo {..} = do
  let rollupAddr = addressFromValidator nid zkirbiRollup
  createDirectoryIfMissing True (takeDirectory fp)
  Yaml.encodeFile fp $
    object
      [ "rollupNFT" .= nonAdaTokenToAssetClass zkirbiNFT
      , "rollupAddr" .= addressToBech32 rollupAddr
      , "rollupScriptRef" .= zkirbiRollupRef
      , "rollupStakeScriptRef" .= zkirbiRollupStakeRef
      , "setupBytes" .= zkrsvcSetupBytes zkirbiRollupStakeValConfig
      , "maxBridgeIn" .= zkrsvcMaxBridgeIn zkirbiRollupStakeValConfig
      , "maxBridgeOut" .= zkrsvcMaxBridgeOut zkirbiRollupStakeValConfig
      , "maxOutputAssets" .= zkrsvcMaxOutputAssets zkirbiRollupStakeValConfig
      ]