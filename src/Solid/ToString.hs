{-# LANGUAGE DefaultSignatures #-}
module Solid.ToString (
  ToString(..)
) where

import           Solid.Common
import           Solid.Types

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import           Data.Text.Encoding (encodeUtf8)
import           Data.Text (Text)
import           Data.Typeable (TypeRep)
import           GHC.Stack (prettyCallStack)

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

instance ToString ()
instance ToString Bool
instance ToString Int
instance ToString Integer
instance ToString Word
instance ToString Float
instance ToString Double

instance Show a => ToString (Maybe a)
instance (Show a, Show b) => ToString (Either a b)

instance (Show a, Show b) => ToString (a, b)
instance (Show a, Show b, Show c) => ToString (a, b, c)
instance (Show a, Show b, Show c, Show d) => ToString (a, b, c, d)
instance (Show a, Show b, Show c, Show d, Show e) => ToString (a, b, c, d, e)
instance (Show a, Show b, Show c, Show d, Show e, Show f) => ToString (a, b, c, d, e, f)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => ToString (a, b, c, d, e, f, g)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => ToString (a, b, c, d, e, f, g, h)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => ToString (a, b, c, d, e, f, g, h, i)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) => ToString (a, b, c, d, e, f, g, h, i, j)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k) => ToString (a, b, c, d, e, f, g, h, i, j, k)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l) => ToString (a, b, c, d, e, f, g, h, i, j, k, l)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m) => ToString (a, b, c, d, e, f, g, h, i, j, k, l, m)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n) => ToString (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) => ToString (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

instance ToString Ordering
instance ToString TypeRep
