{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE UndecidableInstances #-}
module Maybe (
  Maybe(..)

, fold

, nothing?
, just?
) where

import Solid.Common
import Data.Maybe

fold :: b -> (a -> b) -> Maybe a -> b
fold = maybe

nothing? :: Maybe a -> Bool
nothing? = isNothing

just? :: Maybe a -> Bool
just? = isJust

instance HasField "fold" (Maybe a) (b -> (a -> b) -> b)
  => HasField "fold" (Maybe a) (b -> (a -> b) -> b) where
  getField value fl fr = fold fl fr value

instance HasField "nothing\660" (Maybe a) Bool where
  getField = nothing?

instance HasField "just\660" (Maybe a) Bool where
  getField = just?
