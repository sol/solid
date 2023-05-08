{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module List (
  module Data.List
, nub
, nubOn
) where

import Solid.Common
import Data.List hiding (nub, nubBy)
import Data.List qualified as Haskell

import Data.Set qualified as Set

nub :: Ord a => [a] -> [a]
nub = nubOn id

nubOn :: Ord b => (a -> b) -> [a] -> [a]
nubOn f = go Set.empty
  where
    go seen = \ case
      [] -> []
      a : as
        | b `Set.member` seen -> go seen as
        | otherwise -> a : go (Set.insert b seen) as
        where
          b = f a

nub!! :: Eq a => [a] -> [a]
nub!! = Haskell.nub

instance HasField "length" [a] Int where
  getField = length

instance HasField "map" [a] ((a -> b) -> [b]) => HasField "map" [a] ((a -> b) -> [b]) where
  getField xs f = map f xs

instance Eq a => HasField "startsWith" [a] ([a] -> Bool) where
  getField = flip isPrefixOf

instance Eq a => HasField "endsWith" [a] ([a] -> Bool) where
  getField = flip isSuffixOf

instance Eq a => HasField "contains" [a] ([a] -> Bool) where
  getField = flip isInfixOf

instance Eq a => HasField "stripPrefix" [a] ([a] -> Maybe [a]) where
  getField = flip stripPrefix

instance Ord a => HasField "nub" [a] [a] where
  getField = nub

instance (Ord b, HasField "nubOn" [a] ((a -> b) -> [a])) => HasField "nubOn" [a] ((a -> b) -> [a]) where
  getField = flip nubOn

instance Eq a => HasField "nub\7433\7433" [a] [a] where
  getField = nub!!

instance Ord a => HasField "sort" [a] [a] where
  getField = sort

instance (Ord b, HasField "sortOn" [a] ((a -> b) -> [a])) => HasField "sortOn" [a] ((a -> b) -> [a]) where
  getField = flip sortOn
