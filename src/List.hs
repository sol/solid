{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module List (
  module Data.List
, empty
, empty?

, nub
, nubOn

, startsWith
, endsWith
, contains

, lookup!

, enumerate
, enumerateFrom
, List.join
, randomChoice

, for
, for_
, traverse
, traverse_
) where

import Solid.Common hiding (empty, null, traverse)
import Solid.String
use Solid.Bytes
use Solid.StackTrace

import Data.List hiding (nub, nubBy, null, length)
import GHC.OldList as Data.List (null, length)
import Data.List qualified as Haskell
import Data.Set qualified as Set
import System.Random.Stateful qualified as Haskell

use Data.Foldable
use Data.Traversable

empty :: [a]
empty = []

empty? :: [a] -> Bool
empty? = null

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

enumerateFrom :: Int -> [a] -> [(Int, a)]
enumerateFrom n = zip [n..]

select :: (a -> Bool) -> [a] -> [a]
select = filter

discard :: (a -> Bool) -> [a] -> [a]
discard p = filter (not . p)

-- | Join a list of strings.
--
-- Examples:
--
-- >>> List.join ", " ["foo", "bar", "baz"]
-- "foo, bar, baz"
--
-- >>> ["foo", "bar", "baz" :: String].join ", "
-- "foo, bar, baz"
join :: String -> [String] -> String
join = Bytes.intercalate

randomChoice :: [a] -> IO a
randomChoice xs = (xs !!) <$> Haskell.uniformRM (0, pred xs.length) Haskell.globalStdGen

instance HasField "empty\660" [a] Bool where
  getField = empty?

instance HasField "length" [a] Int where
  getField = length

instance HasField "map" [a] ((a -> b) -> [b])
      => HasField "map" [a] ((a -> b) -> [b]) where
  getField xs f = map f xs

instance HasField "reverse" [a] [a] where
  getField = reverse

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith = isPrefixOf

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith = isSuffixOf

contains :: Eq a => [a] -> [a] -> Bool
contains = isInfixOf

lookup! :: WithStackTrace => Eq a => a -> [(a, b)] -> b
lookup! key values = case lookup key values of
  Nothing -> StackTrace.suppress Exception.invalidValue! "invalid key"
  Just a -> a

instance Eq a => HasField "startsWith" [a] ([a] -> Bool) where
  getField = flip startsWith

instance Eq a => HasField "endsWith" [a] ([a] -> Bool) where
  getField = flip endsWith

instance Eq a => HasField "contains" [a] ([a] -> Bool) where
  getField = flip contains

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

instance HasField "enumerateFrom" [a] (Int -> [(Int, a)]) where
  getField = flip enumerateFrom

instance HasField "filter" [a] ((a -> Bool) -> [a]) where
  getField = flip filter

instance HasField "select" [a] ((a -> Bool) -> [a]) where
  getField = flip select

instance HasField "discard" [a] ((a -> Bool) -> [a]) where
  getField = flip discard

instance HasField "join" [String] (String -> String) where
  getField = flip List.join

instance HasField "intersperse" [a] (a -> [a]) where
  getField = flip intersperse

instance HasField "intercalate" [[a]] ([a] -> [a]) where
  getField = flip intercalate

instance HasField "randomChoice" [a] (IO a) where
  getField = randomChoice

traverse :: Applicative m => (a -> m b) -> [a] -> m [b]
traverse = Traversable.traverse

for :: Applicative m => [a] -> (a -> m b) -> m [b]
for = Traversable.for

traverse_ :: Applicative m => (a -> m b) -> [a] -> m ()
traverse_ = Foldable.traverse_

for_ :: Applicative m => [a] -> (a -> m b) -> m ()
for_ = Foldable.for_

instance (HasField "for" [a] ((a -> m b) -> m [b]), Applicative m)
      =>  HasField "for" [a] ((a -> m b) -> m [b]) where
  getField = for

instance (HasField "traverse" [a] ((a -> m b) -> m [b]), Applicative m)
      =>  HasField "traverse" [a] ((a -> m b) -> m [b]) where
  getField = for

instance (HasField "for_" [a] ((a -> m b) -> m ()), Applicative m)
      =>  HasField "for_" [a] ((a -> m b) -> m ()) where
  getField = for_

instance (HasField "traverse_" [a] ((a -> m b) -> m ()), Applicative m)
      =>  HasField "traverse_" [a] ((a -> m b) -> m ()) where
  getField = for_
