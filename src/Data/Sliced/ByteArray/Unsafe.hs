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
