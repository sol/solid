{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module String (
  String
, module String
) where

import Solid.Common
import Solid.Types
import Solid.Types qualified as Types
import Solid.Bytes qualified as Bytes

import Data.Bits ((.&.))
import Data.Coerce (coerce)
import Data.ByteString qualified as Haskell
import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

asByteString :: String -> ByteString
asByteString = Types.asByteString

toText :: String -> Text
toText = Text.decodeUtf8 . unBytes

fromText :: Text -> String
fromText = Bytes . Text.encodeUtf8

instance Show String where
  showsPrec n = showsPrec n . unpack

instance IsString String where
  fromString = pack

length :: String -> Int
length = utf8length . unBytes
  where
    utf8length :: Haskell.ByteString -> Int
    utf8length = Haskell.foldl' (\ n c -> n + f c ) 0
      where
        f :: Word8 -> Int
        f c = if c .&. 0b11000000 == 0b10000000 then 0 else 1

pack :: [Char] -> String
pack = Bytes . Text.encodeUtf8 . Text.pack

unpack :: String -> [Char]
unpack = Text.unpack . Text.decodeUtf8 . unBytes

words :: String -> [String]
words = coerce Char8.words

unwords :: [String] -> String
unwords = coerce Char8.unwords

lines :: String -> [String]
lines = coerce Char8.lines

unlines :: [String] -> String
unlines = coerce Char8.unlines

strip :: String -> String
strip = fromText . Text.strip . toText

isPrefixOf :: String -> String -> Bool
isPrefixOf = Bytes.isPrefixOf

isSuffixOf :: String -> String -> Bool
isSuffixOf = Bytes.isSuffixOf

isInfixOf :: String -> String -> Bool
isInfixOf = coerce Haskell.isInfixOf

stripPrefix :: String -> String -> Maybe String
stripPrefix = coerce Haskell.stripPrefix

stripSuffix :: String -> String -> Maybe String
stripSuffix = coerce Haskell.stripSuffix

startsWith :: String -> String -> Bool
startsWith = Bytes.startsWith

endsWith :: String -> String -> Bool
endsWith = Bytes.endsWith

instance HasField "length" String Int where
  getField = length

instance HasField "pack" [Char] String where
  getField = pack

instance HasField "unpack" String [Char] where
  getField = unpack

instance HasField "words" String [String] where
  getField = words

instance HasField "unwords" [String] String where
  getField = unwords

instance HasField "lines" String [String] where
  getField = lines

instance HasField "unlines" [String] String where
  getField = unlines

instance HasField "strip" String String where
  getField = strip

instance HasField "contains" String (String -> Bool) where
  getField = flip isInfixOf

instance HasField "stripPrefix" String (String -> Maybe String) where
  getField = flip stripPrefix

instance HasField "stripSuffix" String (String -> Maybe String) where
  getField = flip stripSuffix
