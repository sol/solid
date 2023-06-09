{-# LANGUAGE DerivingStrategies #-}
module Solid.Types where

import Solid.Common

import Data.ByteString qualified as Haskell
import Data.ByteString.Short (toShort)
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import System.OsPath (OsPath)
import System.OsString.Internal.Types (OsString(..), PosixString(..))

newtype Bytes a = Bytes { unBytes :: Haskell.ByteString }
  deriving newtype (Eq, Ord, Semigroup, Monoid)

type ByteString = Bytes Word8

data Utf8
type String = Bytes Utf8

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
