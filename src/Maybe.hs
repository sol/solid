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

.fold :: b -> (a -> b) -> Maybe a -> b
.fold = maybe

.nothing? :: Maybe a -> Bool
.nothing? = isNothing

.just? :: Maybe a -> Bool
.just? = isJust
