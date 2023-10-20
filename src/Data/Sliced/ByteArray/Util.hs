{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE CPP #-}
module Data.Sliced.ByteArray.Util (
  ST
, runST
, Array
, MArray
, create
, checkedAdd
, checkedSum
, overflowError
) where

#ifdef x86_64_HOST_ARCH
#define MAX_INT_IS_HUGE
#elif i386_HOST_ARCH
#else
#warning untested ARCH, using checkedAdd as a fallback
#endif

import Solid.Common
import HaskellPrelude (error)
import GHC.Stack (HasCallStack, withFrozenCallStack)

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

overflowError :: HasCallStack => a
overflowError = withFrozenCallStack $ error "size overflow"
