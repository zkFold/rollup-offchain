{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.Rollup.Orphans () where

import Control.Lens ((?~))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..))
import Crypto.Random.Types qualified as Crypto
import Data.Swagger qualified as Swagger
import Data.Swagger.Internal.Schema qualified as Swagger
import GeniusYield.Imports ((&))
import GeniusYield.Swagger.Utils
import GeniusYield.TxBuilder (GYTxBuilderMonadIO, GYTxQueryMonadIO)
import GeniusYield.TxBuilder.IO.Unsafe (unsafeIOToQueryMonad, unsafeIOToTxBuilderMonad)
import GeniusYield.Types hiding (nonAdaTokenFromAssetClass, nonAdaTokenToAssetClass)
import GeniusYield.Types.OpenApi ()
import ZkFold.Protocol.Plonkup.OffChain.Cardano (ByteStringFromHex (..), ZKF (..), ZKProofBytes (..))

instance MonadIO GYTxBuilderMonadIO where
  liftIO = unsafeIOToTxBuilderMonad

instance MonadIO GYTxQueryMonadIO where
  liftIO = unsafeIOToQueryMonad

instance Crypto.MonadRandom GYTxQueryMonadIO where
  getRandomBytes = liftIO . Crypto.getRandomBytes

instance Crypto.MonadRandom m ⇒ Crypto.MonadRandom (ReaderT r m) where
  getRandomBytes n = ReaderT (const $ Crypto.getRandomBytes n)

-- TODO: Move it to Atlas.
instance Swagger.ToSchema GYDatum where
  declareNamedSchema _ =
    pure $
      Swagger.named "GYDatum" $
        mempty & Swagger.description
          ?~ "JSON representation of datum"

-- TODO: Move it to Atlas?
instance Show GYTx where
  show = show . txToApi

instance Swagger.ToSchema ZKF where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Field element."

instance Swagger.ToSchema ByteStringFromHex where
  declareNamedSchema _ =
    pure $
      Swagger.named "ByteStringFromHex" $
        mempty & Swagger.type_
          ?~ Swagger.SwaggerString & Swagger.format
          ?~ "hex"
            & Swagger.description
          ?~ "Bytes encoded in hex."

instance Swagger.ToSchema ZKProofBytes where
  declareNamedSchema =
    Swagger.genericDeclareNamedSchema Swagger.defaultSchemaOptions
      & addSwaggerDescription "Proof bytes where bytes are represented in hexadecimal encoding."