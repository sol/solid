{-# LANGUAGE DataKinds #-}
module Solid.PP.DList where

import           Prelude ()
import           Solid.PP.IO

newtype DList a = DList ([a] -> [a])

instance Semigroup (DList a) where
  DList xs <> DList ys = DList (xs . ys)

instance Monoid (DList a) where
  mempty = DList id

instance HasField "build" (DList a) [a] where
  getField (DList xs) = xs []

concatMap :: Foldable t => (a -> DList b) -> t a -> DList b
concatMap f = foldr ((<>) . f) mempty

singleton :: a -> DList a
singleton = DList . (:)
