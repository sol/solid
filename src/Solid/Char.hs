{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.Char (
  chr
) where

import Solid.Common

import Data.Char

instance HasField "ord" Char Int where
  getField = ord

instance HasField "ascii\660" Char Bool where
  getField c = c < '\128'

instance HasField "valid\660" Char Bool where
  -- https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf
  --
  -- D76 Unicode scalar value:
  --
  -- Any Unicode code point except high-surrogate and low-surrogate code
  -- points.
  --
  -- As a result of this definition, the set of Unicode scalar values consists
  -- of the ranges 0 to 0xD7FF and 0xE000 to 0x10FFFF, inclusive.
  getField c = c < '\xD800' || '\xDFFF' < c
