{-# LANGUAGE AllowAmbiguousTypes #-}

module ZkFold.Cardano.Rollup.Aggregator.Utils (
  ExceptionTypes (..),
  isMatchedException,
  dropSymbolAndCamelToSnake,
  addSwaggerDescription,
  addSwaggerExample,
  bytestringToString,
  tShow,
) where

import Control.Exception (SomeException, fromException)
import Data.ByteString (ByteString)
import Data.Text qualified as Text
import GeniusYield.Imports
import GeniusYield.Swagger.Utils (addSwaggerDescription, addSwaggerExample, dropSymbolAndCamelToSnake)

type ExceptionTypes ∷ [Type] → Type
data ExceptionTypes es where
  ENil ∷ ExceptionTypes '[]
  (:>>) ∷ Exception e ⇒ Proxy e → ExceptionTypes es → ExceptionTypes (e ': es)

infixr 5 :>>

isMatchedException ∷ ExceptionTypes es → SomeException → Bool
isMatchedException ENil _ = False
isMatchedException (etype :>> etypes) se = isJust (f etype) || isMatchedException etypes se
 where
  f ∷ ∀ e. Exception e ⇒ Proxy e → Maybe e
  f _ = fromException @e se

bytestringToString ∷ ByteString → String
bytestringToString = decodeUtf8Lenient >>> Text.unpack

tShow ∷ Show a ⇒ a → Text
tShow = Text.pack . show