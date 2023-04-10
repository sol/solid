{-# LANGUAGE DefaultSignatures #-}
module Solid.ToString (
  ToString(..)
) where

import           Solid.Common

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import           Data.Text.Encoding (encodeUtf8)
import           Data.Text (Text)
import           Data.Typeable (TypeRep)
import           GHC.Stack (prettyCallStack)

import           Solid.ByteString
import           String

class ToString a where
  toString :: a -> String

  default toString :: Show a => a -> String
  toString = pack . show

instance ToString String where
  toString = id

instance ToString [Char] where
  toString = pack

instance ToString Char where
  toString = Bytes . LB.toStrict . Builder.toLazyByteString . Builder.charUtf8

instance ToString Text where
  toString = Bytes . encodeUtf8

instance ToString CallStack where
  toString = pack . prettyCallStack

instance ToString Int
instance ToString TypeRep
