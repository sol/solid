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

fold :: (l -> a) -> (r -> a) -> Either l r -> a
fold = either

.left_or :: l -> Either l r -> l
.left_or = fromLeft

.right_or :: r -> Either l r -> r
.right_or = fromRight

left! :: WithStackTrace => Either l r -> l
left! = \ case
  Left l -> l
  Right _ -> StackTrace.suppress Exception.invalidValue! "Right"

right! :: WithStackTrace => Either l r -> r
right! = \ case
  Left _ -> StackTrace.suppress Exception.invalidValue! "Left"
  Right r -> r

.left? :: Either l r -> Bool
.left? = isLeft

.right? :: Either l r -> Bool
.right? = isRight

instance HasField "fold" (Either l r) ((l -> a) -> (r -> a) -> a)
      => HasField "fold" (Either l r) ((l -> a) -> (r -> a) -> a) where
  getField value fl fr = fold fl fr value

instance HasField "left\7433" (Either l r) l where
  getField = StackTrace.suppressForMethod "Either.left!" left!

instance HasField "right\7433" (Either l r) r where
  getField = StackTrace.suppressForMethod "Either.right!" right!
