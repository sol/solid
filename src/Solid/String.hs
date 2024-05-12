{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.String where

import Solid.Common
import Solid.Bytes.Unsafe

import Data.Coerce (coerce)

use Data.Text.Encoding as Text

use Data.Sliced.ByteArray
use Data.Sliced.ByteArray.Utf8
use Data.Sliced.ByteArray.Conversion

asString :: Bytes a -> Maybe String
asString (Bytes bytes) = if ByteArray.isValidUtf8 bytes then Just (Bytes bytes) else Nothing

decodeUtf8 :: Bytes a -> String
decodeUtf8 input = case asString input of
  Nothing -> Bytes . Conversion.fromText $ Text.decodeUtf8Lenient (Conversion.toByteString $ unBytes input)
  Just xs -> xs

data Utf8
type String = Bytes Utf8

deriving newtype instance Semigroup String
deriving newtype instance Monoid String

instance Show String where
  showsPrec n = showsPrec n . unpack

instance IsString String where
  fromString = pack

pack :: [Char] -> String
pack = coerce Utf8.pack

unpack :: String -> [Char]
unpack = coerce Utf8.unsafeUnpack

singleton :: Char -> String
singleton = coerce Utf8.singleton
