{-# LANGUAGE DerivingStrategies #-}
module Solid.Types where

import Solid.Common

import Data.ByteString qualified as Haskell

newtype Bytes a = Bytes { unBytes :: Haskell.ByteString }
  deriving newtype (Eq, Ord, Semigroup, Monoid)

type ByteString = Bytes Word8

data Utf8
type String = Bytes Utf8

asByteString :: Bytes a -> ByteString
asByteString = Bytes . unBytes

instance HasField "asByteString" (Bytes a) ByteString where
  getField = asByteString
