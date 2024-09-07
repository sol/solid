{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE UndecidableInstances #-}
module Maybe (
  Maybe(..)

, fold

, nothing?
, just?

, foreach
, traverse
) where

import Solid.Common hiding (traverse)
use Solid.Common
use Data.Foldable

import Data.Maybe

.fold :: b -> (a -> b) -> Maybe a -> b
.fold = maybe

.nothing? :: Maybe a -> Bool
.nothing? = isNothing

.just? :: Maybe a -> Bool
.just? = isJust

.foreach :: Applicative m => (a -> m b) -> Maybe a -> m ()
.foreach = Foldable.traverse_

.traverse :: Applicative m => (a -> m b) -> Maybe a -> m (Maybe b)
.traverse = Common.traverse
