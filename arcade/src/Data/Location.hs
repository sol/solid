{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Location where

import Prelude ()
import HaskellPrelude

import           Data.Attoparsec.Text
use Data.Text as T

data Location = Location {
  locationFile :: FilePath
, locationLine :: Int
, locationColumn :: Int
} deriving (Eq, Show)

data Span = Span FilePath (Int, Int) (Int, Int)
  deriving (Eq, Show)

-- spanStart :: Span -> Location
-- spanStart (Span file (l, c) _) = Location file l c

data Located a = Located Location a
  deriving (Eq, Show, Functor, Foldable, Traversable)

{-
unLocated :: Located a -> a
unLocated (Located _ a) = a

locationParser :: Parser Location
locationParser = Location <$> name <*> l <*> c <* skipSpace
  where
    l = decimal <* ":"
    c = decimal <* ":"
    name = T.unpack <$> takeTill (== ':') <* ":"

showLocation :: Location -> String
showLocation (Location file l c) = file <> ":" <> show l <> ":" <> show c <> ":"
-}
