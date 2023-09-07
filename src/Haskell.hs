{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Haskell (
  Haskell.String

, Haskell.ByteString
, asByteString
, fromByteString

, Text
, toText
, fromText

, Haskell.OsPath
, asOsPath
, fromOsPath

, Haskell.PlatformPath
, asPlatformPath
, fromPlatformPath

, Haskell.FilePath
, toFilePath
, toFilePath!
, fromFilePath
, fromFilePath!

, toCallStack
, fromCallStack
) where

import Solid.Common
import Solid.String (String)
import Solid.ByteString (ByteString)
import Solid.Bytes.Unsafe (Bytes(..), FilePath(..))
import Solid.StackTrace (toCallStack, fromCallStack)

import System.IO.Unsafe (unsafePerformIO)
import Data.String qualified as Haskell
import Data.ByteString qualified as Haskell
import System.FilePath qualified as Haskell
import System.OsPath qualified as Haskell
import System.OsPath.Types qualified as Haskell
import System.OsString.Internal.Types (OsString(..))

import Data.Text (Text)
import Data.Text.Encoding qualified as Text

asByteString :: ByteString -> Haskell.ByteString
asByteString = unBytes

fromByteString :: Haskell.ByteString -> ByteString
fromByteString = Bytes

toText :: String -> Text
toText = Text.decodeUtf8 . unBytes

fromText :: Text -> String
fromText = Bytes . Text.encodeUtf8

asOsPath :: FilePath -> Haskell.OsPath
asOsPath = unFilePath

fromOsPath :: Haskell.OsPath -> FilePath
fromOsPath = FilePath

asPlatformPath :: FilePath -> Haskell.PlatformPath
asPlatformPath = getOsString . unFilePath

fromPlatformPath :: Haskell.PlatformPath -> FilePath
fromPlatformPath = FilePath . OsString

toFilePath :: FilePath -> IO Haskell.FilePath
toFilePath = Haskell.decodeFS . unFilePath

toFilePath! :: FilePath -> Haskell.FilePath
toFilePath! = unsafePerformIO . toFilePath

fromFilePath :: Haskell.FilePath -> IO FilePath
fromFilePath = fmap FilePath . Haskell.encodeFS

fromFilePath! :: Haskell.FilePath -> FilePath
fromFilePath! = unsafePerformIO . fromFilePath
