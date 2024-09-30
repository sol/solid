{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ImplicitParams #-}
module Data.Sliced.ByteArray.Util (
  ST
, runST
, Array
, MArray
, copySlice
, checkedAdd
, checkedSum
, checkedMultiply
, overflowError
, withCallStack
) where

import Solid.Common
import GHC.Stack
import GHC.Exts

import Control.Monad.ST (ST, runST)
import Data.Text.Array (MArray, Array)
use Data.List
use Data.Text.Array

copySlice :: MArray s -> Int -> Array -> Int -> Int -> ST s ()
copySlice dst dst_off src src_off len = if len == 0 then pass else Array.copyI len dst dst_off src src_off
{-# INLINE copySlice #-}

checkedAdd :: [Char] -> Int -> Int -> Int
checkedAdd name x y
  | r < 0 = overflowError name
  | otherwise = r
  where r = x + y
{-# INLINE checkedAdd #-}

checkedSum :: [Char] -> [Int] -> Int
checkedSum name = List.foldl' (checkedAdd name) 0
{-# INLINE checkedSum #-}

checkedMultiply :: [Char] -> Int -> Int -> Int
checkedMultiply name !(I# x#) !(I# y#) = case timesInt2# x# y# of
  (# 0#, _, result #) -> I# result
  _ -> overflowError name
{-# INLINE checkedMultiply #-}

overflowError :: [Char] -> a
overflowError name = errorWithoutStackTrace $ "Data.Sliced.ByteArray." <> name <> ": size overflow"

withCallStack :: HasCallStack => (CallStack -> CallStack) -> (HasCallStack => a) -> a
withCallStack f g = let ?callStack = f (popCallStack callStack) in g
