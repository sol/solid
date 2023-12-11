{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Haskell (
  Haskell.String

, Haskell.ByteString
, asByteString
, fromByteString

, Haskell.LazyByteString
, toLazyByteString
, fromLazyByteString

, Text
, toText
, unsafeToText
, fromText

, Haskell.PosixString
, toPosixString
, fromPosixString

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
import Solid.StackTrace (toCallStack, fromCallStack)

import System.IO.Unsafe (unsafePerformIO)
use Data.String as Haskell
use Data.ByteString as Haskell
use Data.ByteString.Lazy as Haskell (LazyByteString)
use System.FilePath as Haskell
use System.OsPath as Haskell
use System.OsPath.Types as Haskell
import System.OsString.Internal.Types (OsString(..))

import Data.Text (Text)

use Data.Sliced.ByteArray.Conversion
import Data.Coerce (coerce)
import Solid.Bytes.Unsafe

asByteString :: Bytes a -> Haskell.ByteString
asByteString = coerce Conversion.toByteString

fromByteString :: Haskell.ByteString -> ByteString
fromByteString = coerce Conversion.fromByteString

toLazyByteString :: Bytes a -> Haskell.LazyByteString
toLazyByteString = coerce Conversion.toLazyByteString
{-# INLINE toLazyByteString #-}

fromLazyByteString :: Haskell.LazyByteString -> ByteString
fromLazyByteString = coerce Conversion.fromLazyByteString
{-# INLINE fromLazyByteString #-}

toText :: String -> Text
toText = coerce Conversion.unsafeToText

unsafeToText :: Bytes a -> Text
unsafeToText = coerce Conversion.unsafeToText

fromText :: Text -> String
fromText = coerce Conversion.fromText

toPosixString :: ByteString -> Haskell.PosixString
toPosixString = coerce Conversion.toPosixString

fromPosixString :: Haskell.PosixString -> ByteString
fromPosixString = coerce Conversion.fromPosixString

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
