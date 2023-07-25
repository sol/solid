{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module List (
  module Data.List
, nub
, nubOn

, enumerate
, randomChoice

, empty
, empty?
) where

import Solid.Common hiding (empty, null)
import Data.List hiding (nub, nubBy, null, length)
import GHC.OldList as Data.List (null, length)
import Data.List qualified as Haskell

import Data.Set qualified as Set
import System.Random.Stateful qualified as Haskell

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

-- | Enumerate a list.
--
-- Examples:
--
-- >>> List.enumerate ["foo", "bar", "baz"]
-- [(0,"foo"),(1,"bar"),(2,"baz")]
--
-- >>> ["foo", "bar", "baz"].enumerate
-- [(0,"foo"),(1,"bar"),(2,"baz")]
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

randomChoice :: [a] -> IO a
randomChoice xs = (xs !!) <$> Haskell.uniformRM (0, pred xs.length) Haskell.globalStdGen

empty :: [a]
empty = []

empty? :: [a] -> Bool
empty? = null

instance HasField "empty\660" [a] Bool where
  getField = empty?

instance HasField "length" [a] Int where
  getField = length

instance HasField "map" [a] ((a -> b) -> [b])
      => HasField "map" [a] ((a -> b) -> [b]) where
  getField xs f = map f xs

instance HasField "reverse" [a] [a] where
  getField = reverse

instance Eq a => HasField "startsWith" [a] ([a] -> Bool) where
  getField = flip isPrefixOf

instance Eq a => HasField "endsWith" [a] ([a] -> Bool) where
  getField = flip isSuffixOf

instance Eq a => HasField "contains" [a] ([a] -> Bool) where
  getField = flip isInfixOf

instance Eq a => HasField "span" [a] ((a -> Bool) -> ([a], [a])) where
  getField = flip span

instance Eq a => HasField "break" [a] ((a -> Bool) -> ([a], [a])) where
  getField = flip break

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

instance HasField "zip" [b] ([a] -> [(a, b)])
      => HasField "zip" [b] ([a] -> [(a, b)]) where
  getField = flip zip

instance HasField "enumerate" [a] [(Int, a)] where
  getField = enumerate

instance HasField "filter" [a] ((a -> Bool) -> [a]) where
  getField = flip filter

instance HasField "randomChoice" [a] (IO a) where
  getField = randomChoice
