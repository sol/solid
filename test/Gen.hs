{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Gen (module Gen) where

import Solid
import Haskell qualified

import Hedgehog
import Hedgehog.Gen as Gen hiding (enum, ascii, unicodeAll, bytes, string)
import Hedgehog.Gen qualified as HedgehogGen
import Range

ascii :: MonadGen m => m Char
ascii = enum '\0' '\127'

-- | Unicode scalar value: Any Unicode code point except high-surrogate and
-- low-surrogate code points.
unicodeScalar :: MonadGen m => m Char
-- D76: https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf
unicodeScalar = frequency [
    (55296,   enum '\x000000' '\x00D7FF')
  , (1056768, enum '\x00E000' '\x10FFFF')
  ]

-- | Any Unicode code point.
--
-- This includes high-surrogate and low-surrogate code points.  Note that
-- surrogate code points can not be encode in UTF-16 and are not allowed in
-- well-formed UTF-8 and UTF-32.
unicodeAny :: MonadGen m => m Char
unicodeAny = choice [
    unicodeScalar
  , surrogate
  ]

surrogate :: MonadGen m => m Char
surrogate = Gen.enum '\xD800' '\xDFFF'

enum :: (MonadGen m, Enum a) => a -> a -> m a
enum lo hi = Gen.choice [
    enumRange Range.exponential lo hi
  , enumRange Range.exponential hi lo
  , enumRange Range.constant lo hi
  ]

enumRange :: (MonadGen m, Enum a) => (Int -> Int -> Range Int) -> a -> a -> m a
enumRange range lo hi =
  fmap toEnum . Gen.integral $
    range (fromEnum lo) (fromEnum hi)

bytes :: MonadGen m => Range Int -> m ByteString
bytes = fmap Haskell.fromByteString . HedgehogGen.bytes

string :: MonadGen m => Range Int -> m Char -> m String
string range = fmap pack . HedgehogGen.list range
