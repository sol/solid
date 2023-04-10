{-# LANGUAGE DerivingStrategies #-}
module Solid.ByteString where

import Solid.Common
import Data.ByteString qualified as Haskell

newtype Bytes a = Bytes { unBytes :: Haskell.ByteString }
  deriving newtype (Eq, Ord, Semigroup, Monoid)

type ByteString = Bytes Word8

instance Show ByteString where
  showsPrec n = showsPrec n . unBytes

asByteString :: Bytes a -> ByteString
asByteString = Bytes . unBytes

instance HasField "asByteString" (Bytes a) ByteString where
  getField = asByteString

instance HasField "length" ByteString Int where
  getField = Haskell.length . unBytes
