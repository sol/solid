{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Annotated (
  Annotated(..)
, empty?
, splitAt
, chunksOf
) where

import Prelude hiding (splitAt)

import GHC.Exts (IsList(..))

newtype Annotated a = Annotated [(a, String)]
  deriving (Eq, Show, Functor)

instance IsList (Annotated a) where
  type Item (Annotated a) = (a, String)
  fromList = Annotated
  toList (Annotated chunks) = chunks

.empty? :: Annotated a -> Bool
.empty? (Annotated chunks) = chunks.empty?

.splitAt :: Int -> Annotated a -> (Annotated a, Annotated a)
.splitAt n = bimap (Annotated . reverse) Annotated . go 0 [] . toList
  where
    go :: Int -> [(a, String)] -> [(a, String)] -> ([(a, String)], [(a, String)])
    go i acc = \ case
      [] -> (acc, [])
      chunks | i == n -> (acc, chunks)
      current@(a, chunk) : chunks
        | j <= n -> go j (current : acc) chunks
        | otherwise -> case chunk.splitAt (n - i) of
            (xs, ys) -> ((a, xs) : acc, (a, ys) : chunks)
        where j = i + chunk.length

.chunksOf :: Int -> Annotated a -> [Annotated a]
.chunksOf n = go
  where
    go (splitAt n -> (as, bs))
      | as.empty? = []
      | otherwise = as : go bs
