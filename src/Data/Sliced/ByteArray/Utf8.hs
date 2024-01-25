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

-- ** Breaking strings
, strip
, ByteArray.stripPrefix
, ByteArray.stripSuffix

-- ** Breaking into many substrings
, split

-- ** Breaking into lines and words
, ByteArray.lines
, words
, ByteArray.unlines
, ByteArray.unwords

-- * Predicates
, ByteArray.isPrefixOf
, ByteArray.isSuffixOf
, ByteArray.isInfixOf

-- * Searching
, elem
, ByteArray.indices -- FIXME: unsafe

, chunksOf
, take
, drop
, splitAt
, ByteArray.breakOn
) where

import Solid.Common hiding (take, drop, splitAt, elem)

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

strip :: ByteArray -> ByteArray
strip = fromText . Text.strip . unsafeToText

chunksOf :: Int -> ByteArray -> [ByteArray]
chunksOf n = map fromText . Text.chunksOf n . unsafeToText

take :: Int -> ByteArray -> ByteArray
take n = fromText . Text.take n . unsafeToText

drop :: Int -> ByteArray -> ByteArray
drop n = fromText . Text.drop n . unsafeToText

splitAt :: Int -> ByteArray -> (ByteArray, ByteArray)
splitAt n = bimap fromText fromText . Text.splitAt n . unsafeToText

elem :: Char -> ByteArray -> Bool
elem c = Text.elem c . unsafeToText

split :: ByteArray -> ByteArray -> [ByteArray]
split needle bytes
  | needle.len == 0 = elements bytes
  | otherwise = ByteArray.split needle bytes

elements :: ByteArray -> [ByteArray]
elements = map singleton . unsafeUnpack -- TODO: implement via measureOff
{-
elements bytes = go 0
  where
    go n
      | n < bytes.len = ByteArray.unsafeSlice n m bytes : go m
      | otherwise = []
      where m = n.succ
      -}

-- measureOff = undefined
