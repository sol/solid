{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FieldSelectors #-}
module Solid.Types where

import Solid.Common

import Data.ByteString qualified as Haskell
import Data.ByteString.Short (toShort)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import System.OsPath (OsPath)
import System.OsString.Internal.Types (OsString(..), PosixString(..))

newtype Bytes a = Bytes { unBytes :: Haskell.ByteString }
  deriving newtype (Eq, Ord)

type ByteString = Bytes Word8

deriving newtype instance Semigroup ByteString
deriving newtype instance Monoid ByteString


asByteString :: Bytes a -> ByteString
asByteString = Bytes . unBytes

instance HasField "asByteString" (Bytes a) ByteString where
  getField = asByteString

asString :: Bytes a -> Maybe String
asString (Bytes string) = if Haskell.isValidUtf8 string then Just (Bytes string) else Nothing

decodeUtf8 :: Bytes a -> String
decodeUtf8 input = case asString input of
  Just xs -> xs
  Nothing -> Bytes . Text.encodeUtf8 $ Text.decodeUtf8With Text.lenientDecode (unBytes input)

newtype FilePath = FilePath { unFilePath :: OsPath }
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid)

asFilePath :: Bytes a -> FilePath
asFilePath = FilePath . OsString . PosixString . toShort . unBytes

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
