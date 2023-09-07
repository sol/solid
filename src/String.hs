{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module String (
  String
, module String
, pack
, unpack
) where

import Solid.Common hiding (empty, replicate)
import Solid.String
import Solid.ByteString (ByteString)
import Solid.Bytes.Unsafe
use Solid.Bytes
use Solid.StackTrace

import Solid.Ansi.Types (Ansi(..))
use Solid.Ansi.Types as Ansi

import Data.Bits ((.&.))
import Data.Coerce (coerce)
import Data.Semigroup
use Data.ByteString as Haskell
use Data.ByteString.Char8
import Data.Text (Text)
use Data.Text
use Data.Text.Encoding as Text
import Text.Read (readMaybe)

asByteString :: String -> ByteString
asByteString = Bytes.asByteString

toText :: String -> Text
toText = Text.decodeUtf8 . unBytes

fromText :: Text -> String
fromText = Bytes . Text.encodeUtf8

empty :: String
empty = mempty

empty? :: Bytes a -> Bool
empty? = coerce Haskell.null

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

-- |
-- >>> let input = "hey-there" :: String
--
-- >>> input.split "-"
-- ["hey","there"]
--
-- >>> input.split ""
-- ["h","e","y","-","t","h","e","r","e"]
split :: String -> String -> [String]
split separator | separator.empty? = map (pack . return) . unpack
split separator = map fromText . Text.splitOn (toText separator) . toText

ljust :: Int -> String -> String
ljust n string = string <> times (n - length string) " "

rjust :: Int -> String -> String
rjust n string = times (n - length string) " " <> string

times :: Int -> String -> String
times n string = if n < 0 then empty else stimes n string

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
asFilePath = Bytes.asFilePath

read :: Read a => String -> Maybe a
read = readMaybe . unpack

read! :: WithStackTrace => Read a => String -> a
read! input = case String.read input of
  Just a -> a
  Nothing -> StackTrace.suppress Exception.invalidValue! "no parse"

ansi :: String -> Ansi String
ansi = Ansi.ansi

instance HasField "empty\660" String Bool where
  getField = empty?

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

instance HasField "split" String (String -> [String]) where
  getField = flip split

instance HasField "ljust" String (Int -> String) where
  getField = flip ljust

instance HasField "rjust" String (Int -> String) where
  getField = flip rjust

instance HasField "times" String (Int -> String) where
  getField = flip times

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
  getField = StackTrace.suppressForMethod "String.read!" String.read!

instance HasField "ansi" String (Ansi String) where
  getField = ansi
