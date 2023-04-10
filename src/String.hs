{-# OPTIONS_GHC -fno-warn-orphans #-}
module String (module String) where

import Solid.Common
import Solid.ByteString
import Solid.String.Type as String

import Data.Coerce (coerce)
import Data.ByteString qualified as Haskell

isPrefixOf :: String -> String -> Bool
isPrefixOf = coerce Haskell.isPrefixOf

isSuffixOf :: String -> String -> Bool
isSuffixOf = coerce Haskell.isSuffixOf

isInfixOf :: String -> String -> Bool
isInfixOf = coerce Haskell.isInfixOf

instance HasField "startsWith" String (String -> Bool) where
  getField = flip isPrefixOf

instance HasField "endsWith" String (String -> Bool) where
  getField = flip isSuffixOf

instance HasField "contains" String (String -> Bool) where
  getField = flip isInfixOf
