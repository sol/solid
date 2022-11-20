{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.String.Type where

import           Prelude ()
import           Solid.Common
import           Solid.ByteString

import           Data.Bits ((.&.))
import           Data.Coerce (coerce)

import qualified Data.ByteString as Haskell
import qualified Data.ByteString.Char8 as Char8

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data Utf8
type String = Bytes Utf8

instance IsString String where
  fromString = pack

instance Show String where
  showsPrec n = showsPrec n . unpack

asString :: Bytes a -> Maybe String
asString (Bytes string) = if Haskell.isValidUtf8 string then Just (Bytes string) else Nothing

instance HasField "asString" (Bytes a) (Maybe String) where
  getField = asString

pack :: [Char] -> String
pack = Bytes . T.encodeUtf8 . T.pack

instance HasField "pack" [Char] String where
  getField = pack

unpack :: String -> [Char]
unpack = T.unpack . T.decodeUtf8 . unBytes

instance HasField "unpack" String [Char] where
  getField = unpack

lines :: String -> [String]
lines = coerce Char8.lines

instance HasField "lines" String [String] where
  getField = lines

unlines :: [String] -> String
unlines = coerce Char8.unlines

instance HasField "unlines" [String] String where
  getField = unlines

instance HasField "length" String Int where
  getField = utf8length . unBytes
    where
      utf8length :: Haskell.ByteString -> Int
      utf8length = Haskell.foldl' (\ n c -> n + f c ) 0
        where
          f c = if c .&. 0b11000000 == 0b10000000 then 0 else 1
