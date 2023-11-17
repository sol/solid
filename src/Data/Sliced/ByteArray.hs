{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Sliced.ByteArray where

import Solid.Common hiding (empty, take, drop, last, tail, init, null, head, splitAt, concat, replicate)
import Data.Semigroup
import Data.List.NonEmpty (NonEmpty)

import GHC.Exts
use Data.List
use Data.Text
use Data.Text.Array
use Simd.Utf8

import Data.Sliced.ByteArray.Util
import Data.Sliced.ByteArray.Unsafe
import Data.Sliced.ByteArray.Conversion

instance Show ByteArray where
  showsPrec :: Int -> ByteArray -> ShowS
  showsPrec n bytes
    | isValidUtf8 bytes = showsPrec n (unsafeToText bytes)
    | otherwise = showsPrec n (compact bytes).arr

instance IsString ByteArray where
  fromString = fromText . Text.pack

instance IsList ByteArray where
  type Item ByteArray = Word8
  fromListN n xs = ByteArray (fromListN n xs) 0 n
  fromList xs = fromListN (List.length xs) xs
  toList = unpack

instance Semigroup ByteArray where
  (<>) = append

  sconcat :: NonEmpty ByteArray -> ByteArray
  sconcat = concat . toList

  stimes :: Integral n => n -> ByteArray -> ByteArray
  stimes n = concat . List.genericReplicate n

instance Monoid ByteArray where
  mempty = empty
  mconcat = concat

pack :: [Word8] -> ByteArray
pack = fromList

unpack :: ByteArray -> [Word8]
unpack bytes = go bytes.off
  where
    !done = bytes.off + bytes.len

    go :: Int -> [Word8]
    go !off = if off < done
      then Array.unsafeIndex bytes.arr off : go (off + 1)
      else []

append :: ByteArray -> ByteArray -> ByteArray
append a b
  | a.len == 0 = b
  | b.len == 0 = a
  | otherwise = ByteArray arr 0 len
  where
    len = a.len `checkedAdd` b.len
    arr = create len $ \ marr -> do
      copyTo marr 0 a
      copyTo marr a.len b

concat :: [ByteArray] -> ByteArray
concat (discardEmpty -> xs) = case xs of
  [] -> empty
  [t] -> t
  _ -> ByteArray arr 0 len
  where
    len = checkedSum $ List.map (.len) xs
    arr = create len $ \ marr -> do
      foldM_ (\ off bytes -> do
        copyTo marr off bytes
        return (off + bytes.len)
        ) 0 xs

discardEmpty :: [ByteArray] -> [ByteArray]
discardEmpty = List.filter (\ x -> x.len > 0)
{-# INLINE discardEmpty #-}

isValidUtf8 :: ByteArray -> Bool
isValidUtf8 ByteArray{..} = Utf8.isValid arr off len

unsafeHead :: ByteArray -> Word8
unsafeHead bytes = Array.unsafeIndex bytes.arr bytes.off
{-# INLINE unsafeHead #-}

unsafeLast :: ByteArray -> Word8
unsafeLast bytes = Array.unsafeIndex bytes.arr (bytes.off + bytes.len - 1)
{-# INLINE unsafeLast #-}
