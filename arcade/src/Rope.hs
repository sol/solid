{-# OPTIONS_GHC -fno-warn-orphans #-}
module Rope (
  Rope
, Position(..)

, Rope.fromString
, Rope.toString
, null

-- * Lines
, lines
, lengthInLines

, takeLines
, dropLines
, splitAtLine

-- * Code points
, length
, splitAt
, lengthAsPosition
, splitAtPosition
) where

import Prelude hiding (null, lines, splitAt)

import Data.Text.Rope (Rope, Position(..))
use Data.Text.Rope as TextRope
use Haskell

-- | O(n)
fromString :: String -> Rope
fromString = TextRope.fromText . Haskell.toText

-- | O(n)
.toString :: Rope -> String
.toString = Haskell.fromText . TextRope.toText

-- | O(1)
.null :: Rope -> Bool
.null = TextRope.null

-- Each line is produced in O(1).
.lines :: Rope -> [String]
.lines = map Haskell.fromText . TextRope.lines

-- | O(log n)
.lengthInLines :: Rope -> Int
.lengthInLines = toInt . TextRope.lengthInLines

-- | O(log n)
.takeLines :: Int -> Rope -> Rope
.takeLines n = fst . splitAtLine n

-- | O(log n)
.dropLines :: Int -> Rope -> Rope
.dropLines n = snd . splitAtLine n

-- | O(log n)
.splitAtLine :: Int -> Rope -> (Rope, Rope)
.splitAtLine = TextRope.splitAtLine . fromInt

-- | O(1)
.length :: Rope -> Int
.length = toInt . TextRope.length

-- | O(n)
.splitAt :: WithStackTrace => Int -> Rope -> (Rope, Rope)
.splitAt = TextRope.splitAt . fromInt

-- | O(1)
.lengthAsPosition :: Rope -> Position
.lengthAsPosition = TextRope.lengthAsPosition

-- Time is linear in 'posColumn' and logarithmic in 'posLine'.
.splitAtPosition :: WithStackTrace => Position -> Rope -> (Rope, Rope)
.splitAtPosition = TextRope.splitAtPosition

toInt :: Word -> Int
toInt = fromIntegral

fromInt :: Int -> Word
fromInt = fromIntegral

instance HasField "line" Position Int where
  getField = toInt . posLine

instance HasField "column" Position Int where
  getField = toInt . posColumn
