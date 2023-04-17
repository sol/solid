{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.Bytes where

import Solid.Common
import Solid.Types

import Data.Coerce (coerce)
import Data.ByteString qualified as Haskell

isPrefixOf :: Bytes a -> Bytes a -> Bool
isPrefixOf = coerce Haskell.isPrefixOf

isSuffixOf :: Bytes a -> Bytes a -> Bool
isSuffixOf = coerce Haskell.isSuffixOf

instance HasField "startsWith" (Bytes a) (Bytes a -> Bool) where
  getField = flip isPrefixOf

instance HasField "endsWith" (Bytes a) (Bytes a -> Bool) where
  getField = flip isSuffixOf
