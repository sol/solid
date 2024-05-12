{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.Bytes.Unsafe where

import Solid.Common

import System.OsPath (OsPath)
import Solid.OsString (OsString(..), PosixString(..))

import Data.Sliced.ByteArray (ByteArray)

newtype Bytes a = Bytes ByteArray
  deriving newtype (Eq, Ord)

unBytes :: Bytes a -> ByteArray
unBytes (Bytes bytes) = bytes

newtype FilePath = FilePath { unFilePath :: OsPath }
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

unFilePath :: FilePath -> OsPath
unFilePath (FilePath bytes) = bytes
