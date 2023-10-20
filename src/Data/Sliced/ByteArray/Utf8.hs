{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Data.Sliced.ByteArray.Utf8 (
  ByteArray

-- * Creation and elimination
, pack
, unsafeUnpack
, singleton
, ByteArray.empty

-- * Basic interface
, ByteArray.null
, length

-- * Substrings
-- ** Breaking into lines and words
, ByteArray.lines
, words
, ByteArray.unlines
, ByteArray.unwords
) where

import Solid.Common

import Data.Sliced.ByteArray.Unsafe (ByteArray(..))
import Data.Sliced.ByteArray.Conversion (unsafeToText, fromText)

use Data.Sliced.ByteArray
use Data.Text
use Simd.Utf8

pack :: [Char] -> ByteArray
pack = fromText . Text.pack

unsafeUnpack :: ByteArray -> [Char]
unsafeUnpack = Text.unpack . unsafeToText

singleton :: Char -> ByteArray
singleton = fromText . Text.singleton

length :: ByteArray -> Int
length bytes = Utf8.length bytes.arr bytes.off bytes.len

words :: ByteArray -> [ByteArray]
words = map fromText . Text.words . unsafeToText
