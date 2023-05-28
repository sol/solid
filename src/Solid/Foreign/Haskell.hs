{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.Foreign.Haskell (
  Haskell.ByteString
, asByteString
, fromByteString

, Haskell.OsPath
, asOsPath
, fromOsPath

, Haskell.FilePath
, toFilePath
, toFilePath!
, fromFilePath
, fromFilePath!
) where

import Solid.Common
import Solid.Types (ByteString, Bytes(..), FilePath(..))

import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString qualified as Haskell
import System.FilePath qualified as Haskell
import System.OsPath qualified as Haskell

asByteString :: ByteString -> Haskell.ByteString
asByteString = unBytes

fromByteString :: Haskell.ByteString -> ByteString
fromByteString = Bytes

asOsPath :: FilePath -> Haskell.OsPath
asOsPath = unFilePath

fromOsPath :: Haskell.OsPath -> FilePath
fromOsPath = FilePath

toFilePath :: FilePath -> IO Haskell.FilePath
toFilePath = Haskell.decodeFS . unFilePath

toFilePath! :: FilePath -> Haskell.FilePath
toFilePath! = unsafePerformIO . toFilePath

fromFilePath :: Haskell.FilePath -> IO FilePath
fromFilePath = fmap FilePath . Haskell.encodeFS

fromFilePath! :: Haskell.FilePath -> FilePath
fromFilePath! = unsafePerformIO . fromFilePath
