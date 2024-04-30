{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module String (
  String
, module String
, pack
, unpack
) where

import Solid.Common hiding (read, empty, replicate)
import Solid.String
import Solid.ByteString (ByteString)
import Solid.Bytes.Unsafe
use Solid.Bytes
use Solid.StackTrace

import Solid.Ansi.Types (Ansi(..))
use Solid.Ansi.Types as Ansi

import Data.Coerce (coerce)
import Data.Semigroup
import Text.Read (readMaybe)

use Data.Sliced.ByteArray.Utf8

asByteString :: String -> ByteString
asByteString = Bytes.asByteString

empty :: String
empty = mempty

.empty? :: String -> Bool
.empty? = coerce Utf8.null

null :: String -> Bool
null = coerce Utf8.null

.length :: String -> Int
.length = coerce Utf8.length

.words :: String -> [String]
.words = coerce Utf8.words

.unwords :: [String] -> String
.unwords = coerce Utf8.unwords

.lines :: String -> [String]
.lines = coerce Utf8.lines

.unlines :: [String] -> String
.unlines = coerce Utf8.unlines

-- |
-- >>> let input = "hey-there" :: String
--
-- >>> input.split "-"
-- ["hey","there"]
--
-- >>> input.split ""
-- ["h","e","y","-","t","h","e","r","e"]
.split :: String -> String -> [String]
.split = coerce Utf8.split

.ljust :: Int -> String -> String
.ljust n string = string <> times (n - length string) " "

.rjust :: Int -> String -> String
.rjust n string = times (n - length string) " " <> string

.times :: Int -> String -> String
.times n string = if n < 0 then empty else stimes n string

.strip :: String -> String
.strip = coerce Utf8.strip

isPrefixOf :: String -> String -> Bool
isPrefixOf = Bytes.isPrefixOf

isSuffixOf :: String -> String -> Bool
isSuffixOf = Bytes.isSuffixOf

isInfixOf :: String -> String -> Bool
isInfixOf = coerce Utf8.isInfixOf

.stripPrefix :: String -> String -> Maybe String
.stripPrefix = coerce Utf8.stripPrefix

.stripSuffix :: String -> String -> Maybe String
.stripSuffix = coerce Utf8.stripSuffix

startsWith :: String -> String -> Bool
startsWith = Bytes.startsWith

endsWith :: String -> String -> Bool
endsWith = Bytes.endsWith

.contains :: String -> String -> Bool
.contains = isInfixOf

.asFilePath :: String -> FilePath
.asFilePath = Bytes.asFilePath

read :: Read a => String -> Maybe a
read = readMaybe . unpack

read! :: WithStackTrace => Read a => String -> a
read! input = case read input of
  Nothing -> StackTrace.suppress Exception.invalidValue! "no parse"
  Just a -> a

.ansi :: String -> Ansi String
.ansi = Ansi.ansi

instance HasField "pack" [Char] String where
  getField = pack

instance HasField "unpack" String [Char] where
  getField = unpack

instance (HasField "read" String (Maybe a), Read a)
       => HasField "read" String (Maybe a) where
  getField = read

instance (HasField "read\7433" String a, Read a)
       => HasField "read\7433" String a where
  getField = StackTrace.suppressForMethod "String.read!" String.read!
