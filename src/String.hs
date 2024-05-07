{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module String (
  String
, module String
, pack
, unpack
) where

import Solid.Common hiding (read, empty, replicate, take, drop, splitAt)
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

-- $setup
-- >>> import Solid

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

-- |
-- >>> String.take 2 "foobar"
-- "fo"
.take :: Int -> String -> String
.take = coerce Utf8.take

-- |
-- >>> String.drop 2 "foobar"
-- "obar"
.drop :: Int -> String -> String
.drop = coerce Utf8.drop

-- |
-- >>> String.takeWhile (/= 'b') "foobar"
-- "foo"
.takeWhile :: (Char -> Bool) -> String -> String
.takeWhile = coerce Utf8.takeWhile

-- |
-- >>> String.dropWhile (/= 'b') "foobar"
-- "bar"
.dropWhile :: (Char -> Bool) -> String -> String
.dropWhile = coerce Utf8.dropWhile

-- |
-- >>> String.takeWhileEnd (/= 'b') "foobar"
-- "ar"
.takeWhileEnd :: (Char -> Bool) -> String -> String
.takeWhileEnd = coerce Utf8.takeWhileEnd

-- |
-- >>> String.dropWhileEnd (/= 'b') "foobar"
-- "foob"
.dropWhileEnd :: (Char -> Bool) -> String -> String
.dropWhileEnd = coerce Utf8.dropWhileEnd

-- |
-- >>> String.splitAt 3 "foobar"
-- ("foo","bar")
.splitAt :: Int -> String -> (String, String)
.splitAt = coerce Utf8.splitAt

-- |
-- >>> String.slice 3 5 "foobar"
-- "ba"
.slice :: Int -> Int -> String -> String
.slice = coerce Utf8.slice

-- |
-- >>> String.chunksOf 2 "foobarbaz"
-- ["fo","ob","ar","ba","z"]
.chunksOf :: Int -> String -> [String]
.chunksOf = coerce Utf8.chunksOf

-- |
-- >>> String.breakOn "ba" "foobarbaz"
-- ("foo","barbaz")
.breakOn :: String -> String -> (String, String)
.breakOn = coerce Utf8.breakOn

.words :: String -> [String]
.words = coerce Utf8.words

-- |
-- >>> String.unwords ["foo", "bar", "baz"]
-- "foo bar baz"
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

.read :: Read a => String -> Maybe a
.read = readMaybe . unpack

.read! :: WithStackTrace => Read a => String -> a
.read! input = case read input of
  Nothing -> StackTrace.suppress Exception.invalidValue! "no parse"
  Just a -> a

.ansi :: String -> Ansi String
.ansi = Ansi.ansi

instance HasField "pack" [Char] String where
  getField = pack

instance HasField "unpack" String [Char] where
  getField = unpack
