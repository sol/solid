{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Int where

import Solid.Common
use Solid.Common

.pred :: Int -> Int
.pred = Common.pred

.pred! :: Int -> Int
.pred! = Common.pred!

.succ :: Int -> Int
.succ = Common.succ

.succ! :: Int -> Int
.succ! = Common.succ!

.negate :: Int -> Int
.negate = Common.negate

.plus :: Int -> Int -> Int
.plus = (+)

.minus :: Int -> Int -> Int
.minus = flip (-)

.times :: Int -> Int -> Int
.times = (*)

.div :: Int -> Int -> Int
.div = Common.div

.min :: Int -> Int -> Int
.min = Common.min

.max :: Int -> Int -> Int
.max = Common.max

.clamp :: Int -> Int -> Int -> Int
.clamp smallest largest n = n.min(largest).max(smallest)
