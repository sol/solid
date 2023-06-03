{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ByteString (
  ByteString
, module ByteString
) where

import Solid.Common
import Solid.Types hiding (asString, decodeUtf8)
import Solid.Types qualified as Types
import Solid.Exception
import Solid.Bytes qualified as Bytes

import Data.Coerce (coerce)
import Data.ByteString qualified as Haskell
import Data.ByteString.Char8 qualified as Char8

instance Show ByteString where
  showsPrec n = showsPrec n . unBytes

instance IsString ByteString where
  fromString = asByteString . String.pack

length :: ByteString -> Int
length = coerce Haskell.length

asString :: ByteString -> Maybe String
asString = Types.asString

asString! :: HasCallStack => ByteString -> String
asString! (Bytes string) = if Haskell.isValidUtf8 string then Bytes string else throw! UnicodeDecodeError

decodeUtf8 :: ByteString -> String
decodeUtf8 = Types.decodeUtf8

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

startsWith :: ByteString -> ByteString -> Bool
startsWith = Bytes.startsWith

endsWith :: ByteString -> ByteString -> Bool
endsWith = Bytes.endsWith

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
