module Solid.PP.Builder (
  Builder
, writeFile
, text

, toText
) where

import           Prelude ()
import           Solid.PP.IO hiding (decodeUtf8, writeFile)

import Data.ByteString.Builder
import qualified Data.ByteString as B
import Data.Text.Encoding (encodeUtf8Builder, decodeUtf8)

text :: Text -> Builder
text = encodeUtf8Builder

toText :: Builder -> Text
toText = decodeUtf8 . B.toStrict . toLazyByteString
