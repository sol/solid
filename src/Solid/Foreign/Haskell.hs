module Solid.Foreign.Haskell (
  Haskell.ByteString
, asByteString
, fromByteString
) where

import Solid.Types qualified as Solid
import Solid.Types (Bytes(..))

import Data.ByteString qualified as Haskell

asByteString :: Solid.ByteString -> Haskell.ByteString
asByteString = unBytes

fromByteString :: Haskell.ByteString -> Solid.ByteString
fromByteString = Bytes
