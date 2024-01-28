{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Data.Sliced.ByteArray.Unsafe where

import Solid.Common hiding (empty)

import Data.Primitive.ByteArray (sizeofByteArray, isByteArrayPinned)

use Data.Text.Array

import Data.Sliced.ByteArray.Util

data ByteArray = ByteArray {
  arr :: {-# UNPACK #-} !Array
, off :: {-# UNPACK #-} !Int
, len :: {-# UNPACK #-} !Int
}

instance Eq ByteArray where
  a == b
    | a.len == b.len = Array.equal a.arr a.off b.arr b.off a.len
    | otherwise = False
  {-# INLINE (==) #-}

instance Ord ByteArray where
  compare :: ByteArray -> ByteArray -> Ordering
  compare a b = Array.compare a.arr a.off b.arr b.off (min a.len b.len) <> compare a.len b.len

empty :: ByteArray
empty = ByteArray mempty 0 0

pin :: ByteArray -> ByteArray
pin bytes
  | isByteArrayPinned bytes.arr = bytes
  | otherwise = ByteArray arr 0 bytes.len
  where
    arr = Array.run $ do
      marr <- Array.newPinned bytes.len
      copyTo marr 0 bytes
      return marr

copy :: ByteArray -> ByteArray
copy bytes = ByteArray arr 0 bytes.len
  where
    arr = create bytes.len $ \ marr -> do
      copyTo marr 0 bytes

compact :: ByteArray -> ByteArray
compact bytes
  | bytes.len == 0 = empty
  | bytes.len == sizeofByteArray bytes.arr = bytes
  | otherwise = copy bytes

copyTo :: MArray s -> Int -> ByteArray -> ST s ()
copyTo marr off bytes = do
  Array.copyI bytes.len marr off bytes.arr bytes.off
{-# INLINE copyTo #-}

unsafeIndex :: Int -> ByteArray -> Word8
unsafeIndex n bytes = Array.unsafeIndex bytes.arr (bytes.off + n)
{-# INLINE unsafeIndex #-}

unsafeHead :: ByteArray -> Word8
unsafeHead bytes = Array.unsafeIndex bytes.arr bytes.off
{-# INLINE unsafeHead #-}

unsafeLast :: ByteArray -> Word8
unsafeLast bytes = Array.unsafeIndex bytes.arr (bytes.off + bytes.len - 1)
{-# INLINE unsafeLast #-}

unsafeTail :: ByteArray -> ByteArray
unsafeTail = unsafeDrop 1
{-# INLINE unsafeTail #-}

unsafeInit :: ByteArray -> ByteArray
unsafeInit = unsafeDropEnd 1
{-# INLINE unsafeInit #-}

unsafeTake :: Int -> ByteArray -> ByteArray
unsafeTake n (ByteArray arr off _) = ByteArray arr off n
{-# INLINE unsafeTake #-}

unsafeTakeEnd :: Int -> ByteArray -> ByteArray
unsafeTakeEnd n (ByteArray arr off len) = ByteArray arr (off + len - n) n
{-# INLINE unsafeTakeEnd #-}

unsafeDrop :: Int -> ByteArray -> ByteArray
unsafeDrop n (ByteArray arr off len) = ByteArray arr (off + n) (len - n)
{-# INLINE unsafeDrop #-}

unsafeDropEnd  :: Int -> ByteArray -> ByteArray
unsafeDropEnd n (ByteArray arr off len) = ByteArray arr off (len - n)
{-# INLINE unsafeDropEnd #-}

unsafeSlice :: Int -> Int -> ByteArray -> ByteArray
unsafeSlice start end (ByteArray arr off _) = ByteArray arr (off + start) (end - start)
{-# INLINE unsafeSlice #-}
