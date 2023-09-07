{-# LANGUAGE DerivingStrategies #-}
module Solid.String where

import Solid.Common
import Solid.Bytes.Unsafe

import Data.ByteString qualified as Haskell
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text

asString :: Bytes a -> Maybe String
asString (Bytes string) = if Haskell.isValidUtf8 string then Just (Bytes string) else Nothing

decodeUtf8 :: Bytes a -> String
decodeUtf8 input = case asString input of
  Just xs -> xs
  Nothing -> Bytes . Text.encodeUtf8 $ Text.decodeUtf8With Text.lenientDecode (unBytes input)

data Utf8
type String = Bytes Utf8

deriving newtype instance Semigroup String
deriving newtype instance Monoid String

instance Show String where
  showsPrec n = showsPrec n . unpack

instance IsString String where
  fromString = pack

pack :: [Char] -> String
pack = Bytes . Text.encodeUtf8 . Text.pack

unpack :: String -> [Char]
unpack = Text.unpack . Text.decodeUtf8 . unBytes
