{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.FilePath (
  FilePath
, fromString
, toString
, asByteString
) where

import Solid.Common hiding (IsString(..))
use Solid.Common
import Solid.Bytes.Unsafe
import Solid.String
import Solid.ByteString (ByteString)
use Solid.Bytes
import Data.ByteString.Short (fromShort)
import System.OsString.Internal.Types (OsString(..), PosixString(..))

fromString :: String -> FilePath
fromString = Bytes.asFilePath

toString :: FilePath -> String
toString = decodeUtf8 . asByteString

asByteString :: FilePath -> ByteString
asByteString = Bytes . fromShort . getPosixString . getOsString . unFilePath

instance Common.IsString FilePath where
  fromString = fromString . String.pack

instance HasField "toString" FilePath String where
  getField = toString

instance HasField "asByteString" FilePath ByteString where
  getField = asByteString
