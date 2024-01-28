{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Data.Sliced.ByteArray.Utf8 (
  ByteArray

-- * Creation and elimination
, pack
, unsafeUnpack
, singleton
, empty

-- * Basic interface
, cons
, snoc
, ByteArray.append
, uncons
, unsnoc
, ByteArray.null
, length


-- * Folds
-- ** Special folds
, ByteArray.concat
, any
, all

-- * Substrings
-- ** Breaking strings
, take
, drop
, splitAt
, ByteArray.breakOn

, ByteArray.stripPrefix
, ByteArray.stripSuffix

, strip

-- ** Breaking into many substrings
, split
, chunksOf

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
) where

import Solid.Common hiding (take, drop, splitAt, elem, any, all, empty)

import Data.Sliced.ByteArray.Unsafe
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

infixr 5 `cons`
infixl 5 `snoc`

cons :: Char -> ByteArray -> ByteArray
cons x = fromText . Text.cons x . unsafeToText

snoc :: ByteArray -> Char -> ByteArray
snoc xs = fromText . Text.snoc (unsafeToText xs)

uncons :: ByteArray -> Maybe (Char, ByteArray)
uncons = fmap (second fromText) . Text.uncons . unsafeToText

unsnoc :: ByteArray -> Maybe (ByteArray, Char)
unsnoc = fmap (first fromText) . Text.unsnoc . unsafeToText

length :: ByteArray -> Int
length bytes = Utf8.length bytes.arr bytes.off bytes.len

any :: (Char -> Bool) -> ByteArray -> Bool
any p = Text.any p . unsafeToText

all :: (Char -> Bool) -> ByteArray -> Bool
all p = Text.all p . unsafeToText

words :: ByteArray -> [ByteArray]
words = map fromText . Text.words . unsafeToText

strip :: ByteArray -> ByteArray
strip = fromText . Text.strip . unsafeToText

take :: Int -> ByteArray -> ByteArray
take n
  | n < 0 = takeEnd -n
  | otherwise = fromText . Text.take n . unsafeToText

drop :: Int -> ByteArray -> ByteArray
drop n
  | n < 0 = dropEnd -n
  | otherwise = fromText . Text.drop n . unsafeToText

takeEnd :: Int -> ByteArray -> ByteArray
takeEnd n = fromText . Text.takeEnd n . unsafeToText

dropEnd :: Int -> ByteArray -> ByteArray
dropEnd n = fromText . Text.dropEnd n . unsafeToText

splitAt :: Int -> ByteArray -> (ByteArray, ByteArray)
splitAt n bytes
  | n < 0 = let m = -n in (dropEnd m bytes, takeEnd m bytes)
  | n == 0 = (empty, bytes)
  | n >= bytes.len || off == bytes.len || off < 0 = (bytes, empty)
  | otherwise = (unsafeTake off bytes, unsafeDrop off bytes)
  where
    off = measureOff n bytes

elem :: Char -> ByteArray -> Bool
elem c = Text.elem c . unsafeToText

split :: ByteArray -> ByteArray -> [ByteArray]
split needle bytes
  | needle.len == 0 = elements bytes
  | otherwise = ByteArray.split needle bytes

elements :: ByteArray -> [ByteArray]
elements = go
  where
    go bytes
      | bytes.len > 0 = unsafeTake off bytes : go (unsafeDrop off bytes)
      | otherwise = []
      where off = measureOff 1 bytes

measureOff :: Int -> ByteArray -> Int
measureOff n = Text.measureOff n . unsafeToText

chunksOf :: Int -> ByteArray -> [ByteArray]
chunksOf n = map fromText . Text.chunksOf n . unsafeToText
