{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.FilePath where

import           Solid.Common
import           Solid.String.Type
import           Solid.ToString
import           Data.Coerce

import qualified System.FilePath as Haskell
import qualified System.Directory as Haskell

newtype FilePath = FilePath { unFilePath :: Haskell.FilePath }
  deriving newtype (Eq, Ord, Show, IsString, ToString)

(</>) :: FilePath -> FilePath -> FilePath
(</>) = coerce (Haskell.</>)

instance HasField "toFilePath" [Char] FilePath where
  getField = FilePath

instance HasField "toFilePath" String FilePath where
  getField = FilePath . unpack

instance HasField "toString" FilePath String where
  getField = toString

instance HasField "exists\660" FilePath (IO Bool) where
  getField = Haskell.doesPathExist . unFilePath
