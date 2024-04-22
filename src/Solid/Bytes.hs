{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.Bytes (
  Bytes
, module Solid.Bytes
) where

import Solid.Common
import Solid.Bytes.Unsafe
import Solid.ByteString

import Data.Coerce (coerce)

use Data.Sliced.ByteArray
use Data.Sliced.ByteArray.Conversion

intercalate :: Bytes a -> [Bytes a] -> Bytes a
intercalate = coerce ByteArray.intercalate

isPrefixOf :: Bytes a -> Bytes a -> Bool
isPrefixOf = coerce ByteArray.isPrefixOf

isSuffixOf :: Bytes a -> Bytes a -> Bool
isSuffixOf = coerce ByteArray.isSuffixOf

.startsWith :: Bytes a -> Bytes a -> Bool
.startsWith = isPrefixOf

.endsWith :: Bytes a -> Bytes a -> Bool
.endsWith = isSuffixOf

asFilePath :: Bytes a -> FilePath
asFilePath = FilePath . coerce Conversion.toOsPath

.asByteString :: Bytes a -> ByteString
.asByteString = Bytes . unBytes
