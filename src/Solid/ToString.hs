{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.ToString (
  ToString(..)
) where

import           Prelude ()
import           Solid.Common

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB

import           Solid.String

class ToString a where
  toString :: a -> String

  default toString :: Show a => a -> String
  toString = pack . show

instance ToString String where
  toString = id

instance ToString Char where
  toString = Bytes . LB.toStrict . Builder.toLazyByteString . Builder.charUtf8

instance ToString Int
