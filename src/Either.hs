{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Either (
  Either(..)

, fold

, left_or
, right_or

, left!
, right!

, left?
, right?
) where

import Solid.Common
import Solid.StackTrace qualified as StackTrace
import Data.Either

.fold :: (l -> a) -> (r -> a) -> Either l r -> a
.fold = either

.left_or :: l -> Either l r -> l
.left_or = fromLeft

.right_or :: r -> Either l r -> r
.right_or = fromRight

.left! :: WithStackTrace => Either l r -> l
.left! = \ case
  Left l -> l
  Right _ -> StackTrace.suppress Exception.invalidValue! "Right"

.right! :: WithStackTrace => Either l r -> r
.right! = \ case
  Left _ -> StackTrace.suppress Exception.invalidValue! "Left"
  Right r -> r

.left? :: Either l r -> Bool
.left? = isLeft

.right? :: Either l r -> Bool
.right? = isRight
