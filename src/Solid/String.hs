{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.String (
  Bytes(..)
, String
, ByteString
, pack
, unpack
, lines
, unlines
) where

import           Prelude ()
import           Solid.Common
import           Solid.Exception

import           Data.Bits ((.&.))
import           Data.Word (Word8)
import           Data.Coerce (coerce)

import qualified Data.ByteString as Haskell
import qualified Data.ByteString.Char8 as Char8

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype Bytes a = Bytes { unBytes :: Haskell.ByteString }
  deriving newtype (Eq, Semigroup, Monoid)

type ByteString = Bytes Word8

instance Show ByteString where
  showsPrec n = showsPrec n . unBytes

data Utf8

type String = Bytes Utf8

asByteString :: Bytes a -> ByteString
asByteString = Bytes . unBytes

instance HasField "asByteString" (Bytes a) ByteString where
  getField = asByteString

asString :: Bytes a -> Maybe String
asString (Bytes string) = if Haskell.isValidUtf8 string then Just (Bytes string) else Nothing

instance HasField "asString" (Bytes a) (Maybe String) where
  getField = asString

asString! :: HasCallStack => Bytes a -> String
asString! (Bytes string) = if Haskell.isValidUtf8 string then Bytes string else throw UnicodeDecodeError

instance HasField "asString\7433" (Bytes a) String where
  getField = asString!

unpack :: String -> [Char]
unpack = T.unpack . T.decodeUtf8 . unBytes

pack :: [Char] -> String
pack = Bytes . T.encodeUtf8 . T.pack

lines :: String -> [String]
lines = coerce Char8.lines

unlines :: [String] -> String
unlines = coerce Char8.unlines

instance Show String where
  showsPrec n = showsPrec n . unpack

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
