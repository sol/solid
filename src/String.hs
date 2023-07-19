{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module String (
  String
, module String
, pack
, unpack
) where

import Solid.Common
import Solid.Types hiding (asFilePath)
import Solid.Types qualified as Types
import Solid.Bytes qualified as Bytes
import Solid.StackTrace qualified as StackTrace

import Data.Bits ((.&.))
import Data.Coerce (coerce)
import Data.ByteString qualified as Haskell
import Data.ByteString.Char8 qualified as Char8
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Text.Read (readMaybe)

asByteString :: String -> ByteString
asByteString = Types.asByteString

toText :: String -> Text
toText = Text.decodeUtf8 . unBytes

fromText :: Text -> String
fromText = Bytes . Text.encodeUtf8

length :: String -> Int
length = utf8length . unBytes
  where
    utf8length :: Haskell.ByteString -> Int
    utf8length = Haskell.foldl' (\ n c -> n + f c ) 0
      where
        f :: Word8 -> Int
        f c = if c .&. 0b11000000 == 0b10000000 then 0 else 1

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

contains :: String -> String -> Bool
contains = isInfixOf

asFilePath :: String -> FilePath
asFilePath = Types.asFilePath

read :: Read a => String -> Maybe a
read = readMaybe . unpack

read! :: WithStackTrace => Read a => String -> a
read! input = case String.read input of
  Just a -> a
  Nothing -> StackTrace.suppress Exception.invalidValue! "no parse"

-- | Join a list of strings.
--
-- Examples:
--
-- >>> String.join ", " ["foo", "bar", "baz"]
-- "foo, bar, baz"
--
-- >>> ["foo", "bar", "baz" :: String].join ", "
-- "foo, bar, baz"
join :: String -> [String] -> String
join = coerce Haskell.intercalate

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
  getField = flip contains

instance HasField "stripPrefix" String (String -> Maybe String) where
  getField = flip stripPrefix

instance HasField "stripSuffix" String (String -> Maybe String) where
  getField = flip stripSuffix

instance HasField "asFilePath" String FilePath where
  getField = asFilePath

instance (HasField "read" String (Maybe a), Read a)
       => HasField "read" String (Maybe a) where
  getField = String.read

instance (HasField "read\7433" String a, Read a)
       => HasField "read\7433" String a where
  getField = StackTrace.suppress String.read!

instance HasField "join" [String] (String -> String) where
  getField = flip String.join
