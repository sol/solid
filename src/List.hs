{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module List (
  module Data.List
, empty
, empty?

, nub
, nubOn
, nub!!

, startsWith
, endsWith
, contains

, lookup!

, enumerate
, enumerateFrom
, randomChoice

, select
, discard

, foreach
, traverse
) where

import Solid.Common hiding (empty, null, traverse)
use Solid.Common
use Data.Foldable

use Solid.StackTrace

import Data.List hiding (nub, nubBy, null, length)
import GHC.OldList as Data.List (null, length)
import Data.List qualified as Haskell
import Data.Set qualified as Set
import System.Random.Stateful qualified as Haskell

empty :: [a]
empty = []

.empty? :: [a] -> Bool
.empty? = null

.nub :: Ord a => [a] -> [a]
.nub = nubOn id

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

.nub!! :: Eq a => [a] -> [a]
.nub!! = Haskell.nub

-- | Enumerate a list.
--
-- Examples:
--
-- >>> List.enumerate ["foo", "bar", "baz"]
-- [(0,"foo"),(1,"bar"),(2,"baz")]
--
-- >>> ["foo", "bar", "baz"].enumerate
-- [(0,"foo"),(1,"bar"),(2,"baz")]
.enumerate :: [a] -> [(Int, a)]
.enumerate = zip [0..]

.enumerateFrom :: Int -> [a] -> [(Int, a)]
.enumerateFrom n = zip [n..]

.select :: (a -> Bool) -> [a] -> [a]
.select = filter

.discard :: (a -> Bool) -> [a] -> [a]
.discard p = filter (not . p)

.randomChoice :: [a] -> IO a
.randomChoice xs = (xs !!) <$> Haskell.uniformRM (0, pred xs.length) Haskell.globalStdGen

instance HasField "length" [a] Int where
  getField = length

instance HasField "map" [a] ((a -> b) -> [b])
      => HasField "map" [a] ((a -> b) -> [b]) where
  getField xs f = map f xs

instance HasField "reverse" [a] [a] where
  getField = reverse

.startsWith :: Eq a => [a] -> [a] -> Bool
.startsWith = isPrefixOf

.endsWith :: Eq a => [a] -> [a] -> Bool
.endsWith = isSuffixOf

.contains :: Eq a => [a] -> [a] -> Bool
.contains = isInfixOf

lookup! :: WithStackTrace => Eq a => a -> [(a, b)] -> b
lookup! key values = case lookup key values of
  Nothing -> StackTrace.suppress Exception.invalidValue! "invalid key"
  Just a -> a

instance Eq a => HasField "lookup" [(a, b)] (a -> Maybe b) where
  getField = flip lookup

instance Eq a => HasField "lookup\7433" [(a, b)] (a -> b) where
  getField = StackTrace.suppressForMethod "List.lookup!" flip List.lookup!

instance Eq a => HasField "span" [a] ((a -> Bool) -> ([a], [a])) where
  getField = flip span

instance Eq a => HasField "break" [a] ((a -> Bool) -> ([a], [a])) where
  getField = flip break

instance Eq a => HasField "stripPrefix" [a] ([a] -> Maybe [a]) where
  getField = flip stripPrefix

instance (Ord b, HasField "nubOn" [a] ((a -> b) -> [a]))
      =>         HasField "nubOn" [a] ((a -> b) -> [a]) where
  getField = flip nubOn

instance Ord a => HasField "sort" [a] [a] where
  getField = sort

instance (Ord b, HasField "sortOn" [a] ((a -> b) -> [a])) => HasField "sortOn" [a] ((a -> b) -> [a]) where
  getField = flip sortOn

instance HasField "zip" [b] ([a] -> [(a, b)])
      => HasField "zip" [b] ([a] -> [(a, b)]) where
  getField = flip zip

instance HasField "filter" [a] ((a -> Bool) -> [a]) where
  getField = flip filter

instance HasField "intersperse" [a] (a -> [a]) where
  getField = flip intersperse

instance HasField "intercalate" [[a]] ([a] -> [a]) where
  getField = flip intercalate

.foreach :: Applicative m => (a -> m b) -> [a] -> m ()
.foreach = Foldable.traverse_

.traverse :: Applicative m => (a -> m b) -> [a] -> m [b]
.traverse = Common.traverse
