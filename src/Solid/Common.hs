{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.Common (
  module Imports
, pass
) where

import           HaskellPrelude as Imports hiding (FilePath, String, words, unwords, lines, unlines, print, readFile, writeFile, error, length)
import           Data.Functor as Imports ((<&>))
import           Data.Bifunctor as Imports
import           Data.String as Imports (IsString(..))
import           Data.Word as Imports (Word8)
import           GHC.Stack as Imports (CallStack)
import           GHC.Records as Imports (HasField(..))
import           GHC.Generics as Imports (Generic)
import           Control.Monad as Imports
import           Control.Monad.IO.Class as Imports
import           Control.Applicative as Imports

import           Data.Tuple (swap)

pass :: Applicative m => m ()
pass = pure ()

instance HasField "curry" ((a, b) -> c) (a -> b -> c) where
  getField = curry

instance HasField "uncurry" (a -> b -> c) ((a, b) -> c) where
  getField = uncurry

instance HasField "flip" (a -> b -> c) (b -> a -> c) where
  getField = flip

instance HasField "swap" (a, b) (b, a) where
  getField = swap
