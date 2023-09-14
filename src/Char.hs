{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Char (
  chr

, ord

, replicate

, valid?
, ascii?
, space?
) where

import Solid.Common hiding (replicate)
import Solid.Types

import Data.Char

valid? :: Char -> Bool
-- https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf
--
-- D76 Unicode scalar value:
--
-- Any Unicode code point except high-surrogate and low-surrogate code
-- points.
--
-- As a result of this definition, the set of Unicode scalar values consists
-- of the ranges 0 to 0xD7FF and 0xE000 to 0x10FFFF, inclusive.
valid? c = c < '\xD800' || '\xDFFF' < c

ascii? :: Char -> Bool
ascii? c = c < '\128'

space? :: Char -> Bool
space? = isSpace

instance HasField "ord" Char Int where
  getField = ord

instance HasField "valid\660" Char Bool where
  getField = valid?

instance HasField "ascii\660" Char Bool where
  getField = ascii?

instance HasField "space\660" Char Bool where
  getField = space?

replicate :: Int -> Char -> String
replicate n = String.times n . pack . return

instance HasField "replicate" Char (Int -> String) where
  getField = flip  replicate
