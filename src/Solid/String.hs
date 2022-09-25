{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.String (
  String
, Bytes(..)
, pack
, unpack
, lines
, unlines
) where

import           Prelude ()
import           Solid.Common

import           Data.Bits ((.&.))
import           Data.Coerce (coerce)

import qualified Data.ByteString as Haskell
import qualified Data.ByteString.Char8 as Char8

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype Bytes a = Bytes { unBytes :: Haskell.ByteString }
  deriving newtype (Eq, Semigroup, Monoid)

data Utf8

type String = Bytes Utf8

unpack :: String -> [Char]
unpack = T.unpack . T.decodeUtf8 . unBytes

pack :: [Char] -> String
pack = Bytes . T.encodeUtf8 . T.pack

lines :: String -> [String]
lines = coerce Char8.lines

unlines :: [String] -> String
unlines = coerce Char8.unlines

instance Show String where
  show = show . unpack

instance IsString String where
  fromString = pack

instance HasField "pack" [Char] String where
  getField = pack

instance HasField "unpack" String [Char] where
  getField = unpack

instance HasField "lines" String [String] where
  getField = lines

instance HasField "unlines" [String] String where
  getField = unlines

instance HasField "length" String Int where
  getField = utf8length . unBytes
    where
      utf8length :: Haskell.ByteString -> Int
      utf8length = Haskell.foldl' (\ n c -> n + f c ) 0
        where
          f c = if c .&. 0b11000000 == 0b10000000 then 0 else 1
