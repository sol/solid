{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Data.Sliced.ByteArray.Common where

import Solid.Common

use Data.List

import Data.Sliced.ByteArray.Util
import Data.Sliced.ByteArray.Unsafe

split :: ByteArray -> Int -> [Int] -> [ByteArray]
split bytes len = go 0
  where
    go :: Int -> [Int] -> [ByteArray]
    go off = \ case
      [] -> [unsafeDrop off bytes]
      n : xs -> unsafeSlice off n bytes : go (n + len) xs
{-# INLINE split #-}

replace :: [Int] -> Int -> ByteArray -> ByteArray -> ByteArray
replace ixs old_len new bytes
  | List.null ixs = bytes
  | otherwise = arr
  where
    len :: Int
    len = bytes.len - checkedMultiply "replace" (old_len - new.len) (List.length ixs)

    arr :: ByteArray
    arr = create len $ \ marr -> do
      let
        go offset_old offset_new = \ case
          ix_old : ixs_old -> do
            let delta = ix_old - offset_old
            copySlice marr offset_new bytes.arr (bytes.off + offset_old) delta
            let ix_new = offset_new + delta
            copyTo marr ix_new new
            go (ix_old + old_len) (ix_new + new.len) ixs_old
          [] -> do
            copySlice marr offset_new bytes.arr (bytes.off + offset_old) (len - offset_new)
      go 0 0 ixs
{-# INLINE replace #-}
