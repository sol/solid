{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Char (
  chr

, ord

, replicate

, valid?
, ascii?
, space?

, lower?
, upper?

, toLower
, toUpper
, toTitle
) where

import Solid.Common hiding (replicate)
import Solid.String

import Data.Char

.valid? :: Char -> Bool
-- https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf
--
-- D76 Unicode scalar value:
--
-- Any Unicode code point except high-surrogate and low-surrogate code
-- points.
--
-- As a result of this definition, the set of Unicode scalar values consists
-- of the ranges 0 to 0xD7FF and 0xE000 to 0x10FFFF, inclusive.
.valid? c = c < '\xD800' || '\xDFFF' < c

.ascii? :: Char -> Bool
.ascii? c = c < '\128'

.space? :: Char -> Bool
.space? = isSpace

.lower? :: Char -> Bool
.lower? = isLower

.upper? :: Char -> Bool
.upper? = isUpper

instance HasField "ord" Char Int where
  getField = ord

.replicate :: Int -> Char -> String
.replicate n = String.times n . pack . return

instance HasField "toLower" Char Char where
  getField = toLower

instance HasField "toUpper" Char Char where
  getField = toUpper

instance HasField "toTitle" Char Char where
  getField = toTitle
