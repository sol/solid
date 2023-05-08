{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ByteString (
  Bytes(..)
, ByteString
, module ByteString
) where

import Solid.Common
import Solid.Types
import Solid.Exception
import Solid.Bytes qualified as Bytes
import String qualified

import Data.Coerce (coerce)
import Data.ByteString qualified as Haskell
import Data.ByteString.Char8 qualified as Char8
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text

instance Show ByteString where
  showsPrec n = showsPrec n . unBytes

instance IsString ByteString where
  fromString = asByteString . String.pack

length :: ByteString -> Int
length = coerce Haskell.length

asString :: ByteString -> Maybe String
asString (Bytes string) = if Haskell.isValidUtf8 string then Just (Bytes string) else Nothing

asString! :: HasCallStack => ByteString -> String
asString! (Bytes string) = if Haskell.isValidUtf8 string then Bytes string else throw! UnicodeDecodeError

decodeUtf8 :: ByteString -> String
decodeUtf8 input = case input.asString of
  Just xs -> xs
  Nothing -> Bytes . Text.encodeUtf8 $ Text.decodeUtf8With Text.lenientDecode (unBytes input)

pack :: [Word8] -> ByteString 
pack = Bytes . Haskell.pack

unpack :: ByteString -> [Word8]
unpack = Haskell.unpack . unBytes

strip :: ByteString -> ByteString
strip = coerce Char8.strip

isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf = Bytes.isPrefixOf

isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf = Bytes.isSuffixOf

isInfixOf :: ByteString -> ByteString -> Bool
isInfixOf = coerce Haskell.isInfixOf

stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix = coerce Haskell.stripPrefix

stripSuffix :: ByteString -> ByteString -> Maybe ByteString
stripSuffix = coerce Haskell.stripSuffix

instance HasField "length" ByteString Int where
  getField = length

instance HasField "asString" ByteString (Maybe String) where
  getField = asString

instance HasField "asString\7433" ByteString String where
  getField = asString!

instance HasField "decodeUtf8" ByteString String where
  getField = decodeUtf8

instance HasField "pack" [Word8] ByteString where
  getField = pack

instance HasField "unpack" ByteString [Word8] where
  getField = unpack

instance HasField "strip" ByteString ByteString where
  getField = strip

instance HasField "contains" ByteString (ByteString -> Bool) where
  getField = flip isInfixOf

instance HasField "stripPrefix" ByteString (ByteString -> Maybe ByteString) where
  getField = flip stripPrefix

instance HasField "stripSuffix" ByteString (ByteString -> Maybe ByteString) where
  getField = flip stripSuffix
