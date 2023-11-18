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
, create
, checkedAdd
, checkedSum
, checkedMultiply
, overflowError
, withCallStack
) where

#ifdef x86_64_HOST_ARCH
#define MAX_INT_IS_HUGE
#elif i386_HOST_ARCH
#else
#warning untested ARCH, using checkedAdd as a fallback
#endif

import Solid.Common
import HaskellPrelude (error)
import GHC.Stack
import GHC.Exts

import Control.Monad.ST (ST, runST)
import Data.Text.Array (MArray, Array)
use Data.Text.Array

create :: Int -> (forall s. MArray s -> ST s ()) -> Array
create !len action = Array.run $ do
  marr <- Array.new len
  action marr
  return marr
{-# INLINE create #-}

#ifdef MAX_INT_IS_HUGE
checkedAdd :: Int -> Int -> Int
checkedAdd = (+)
#else
checkedAdd :: HasCallStack => Int -> Int -> Int
checkedAdd x y
  | r < 0 = overflowError
  | otherwise = r
  where r = x + y
#endif
{-# INLINE checkedAdd #-}

#ifdef MAX_INT_IS_HUGE
checkedSum :: [Int] -> Int
checkedSum = sum
#else
checkedSum :: HasCallStack => [Int] -> Int
checkedSum = List.foldl' checkedAdd 0
#endif
{-# INLINE checkedSum #-}

checkedMultiply :: HasCallStack => Int -> Int -> Int
checkedMultiply !(I# x#) !(I# y#) = case timesInt2# x# y# of
  (# 0#, _, result #) -> I# result
  _ -> overflowError
{-# INLINE checkedMultiply #-}

overflowError :: HasCallStack => a
overflowError = withFrozenCallStack $ error "size overflow"

withCallStack :: HasCallStack => (CallStack -> CallStack) -> (HasCallStack => a) -> a
withCallStack f g = let ?callStack = f (popCallStack callStack) in g
