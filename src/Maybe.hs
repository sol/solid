{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE UndecidableInstances #-}
module Maybe (
  Maybe(..)

, fold

, nothing?
, just?

, map

, foreach
, traverse
, sequence
) where

import Solid.Common hiding (map, traverse, sequence)
use Solid.Common
use Data.Foldable

import Data.Maybe

.fold :: b -> (a -> b) -> Maybe a -> b
.fold = maybe

.nothing? :: Maybe a -> Bool
.nothing? = isNothing

.just? :: Maybe a -> Bool
.just? = isJust

.map :: (a -> b) -> Maybe a -> Maybe b
.map = fmap

.foreach :: Applicative m => (a -> m b) -> Maybe a -> m ()
.foreach = Foldable.traverse_

.traverse :: Applicative m => (a -> m b) -> Maybe a -> m (Maybe b)
.traverse = Common.traverse

.sequence :: Applicative m => Maybe (m a) -> m (Maybe a)
.sequence = sequenceA
