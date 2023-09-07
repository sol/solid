{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}
module Solid.Bytes.Unsafe where

import Solid.Common

import Data.ByteString qualified as Haskell
import System.OsPath (OsPath)
import System.OsString.Internal.Types (OsString(..), PosixString(..))

newtype Bytes a = Bytes { unBytes :: Haskell.ByteString }
  deriving newtype (Eq, Ord)

newtype FilePath = FilePath { unFilePath :: OsPath }
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)
