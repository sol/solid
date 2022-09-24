{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
module Solid.String (
  String
, pack
, unpack
) where

import           Prelude hiding (String)

import           Data.Bits ((.&.))
import           Data.String (IsString(..))
import           GHC.Records (HasField(..))

import qualified Data.ByteString as Haskell

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

instance Show String where
  show = show . unpack

instance IsString String where
  fromString = pack

instance HasField "unpack" String [Char] where
  getField = unpack

instance HasField "length" String Int where
  getField = utf8length . unBytes
    where
      utf8length :: Haskell.ByteString -> Int
      utf8length = Haskell.foldl' (\ n c -> n + f c ) 0
        where
          f c = if c .&. 0b11000000 == 0b10000000 then 0 else 1
