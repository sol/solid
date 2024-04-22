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
use Data.Sliced.ByteArray.Conversion

fromString :: String -> FilePath
fromString = Bytes.asFilePath

.toString :: FilePath -> String
.toString = decodeUtf8 . asByteString

.asByteString :: FilePath -> ByteString
.asByteString = Bytes . Conversion.fromOsPath . unFilePath

instance Common.IsString FilePath where
  fromString = fromString . String.pack
