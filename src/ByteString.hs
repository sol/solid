{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module ByteString (
  ByteString
, module ByteString
) where

import Solid.Common hiding (read, take, drop, takeWhile, dropWhile, splitAt)
import Solid.String (String)
import Solid.ByteString
import Solid.Bytes.Unsafe
import Exception
use Solid.Bytes
use Solid.String
use String
use Solid.StackTrace

import Data.Coerce (coerce)
import Data.ByteString qualified as Haskell
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Internal (isSpaceWord8)

instance Show ByteString where
  showsPrec n = showsPrec n . unBytes

instance IsString ByteString where
  fromString = Bytes.asByteString . String.pack

asString :: ByteString -> Maybe String
asString = String.asString

asString! :: WithStackTrace => ByteString -> String
asString! (Bytes string) = if Haskell.isValidUtf8 string then Bytes string else throw! UnicodeDecodeError

decodeUtf8 :: ByteString -> String
decodeUtf8 = String.decodeUtf8

instance HasField "asString" ByteString (Maybe String) where
  getField = asString

instance HasField "asString\7433" ByteString String where
  getField = asString!

instance HasField "decodeUtf8" ByteString String where
  getField = decodeUtf8

length :: ByteString -> Int
length = coerce Haskell.length

pack :: [Word8] -> ByteString
pack = Bytes . Haskell.pack

unpack :: ByteString -> [Word8]
unpack = Haskell.unpack . unBytes

take :: Int -> ByteString -> ByteString
take = coerce Haskell.take

takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile = coerce Haskell.takeWhile

takeWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhileEnd = coerce Haskell.takeWhileEnd

drop :: Int -> ByteString -> ByteString
drop = coerce Haskell.drop

dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile = coerce Haskell.dropWhile

dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileEnd = coerce Haskell.dropWhileEnd

splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt = coerce Haskell.splitAt

words :: ByteString -> [ByteString]
words = coerce Char8.words

unwords :: [ByteString] -> ByteString
unwords = coerce Char8.unwords

lines :: ByteString -> [ByteString]
lines = coerce Char8.lines

unlines :: [ByteString] -> ByteString
unlines = coerce Char8.unlines

strip :: ByteString -> ByteString
strip = dropWhile asciiSpace? . dropWhileEnd asciiSpace?
  where
    asciiSpace? c = c < 128 && isSpaceWord8 c

inits :: ByteString -> [ByteString]
inits = coerce Haskell.inits

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

contains :: ByteString -> ByteString -> Bool
contains = isInfixOf

asFilePath :: ByteString -> FilePath
asFilePath = Bytes.asFilePath

read :: Read a => ByteString -> Maybe a
read = asString >=> String.read

read! :: WithStackTrace => Read a => ByteString -> a
read! input = case read input of
  Nothing -> StackTrace.suppress Exception.invalidValue! "no parse"
  Just a -> a

instance HasField "length" ByteString Int where
  getField = length

instance HasField "pack" [Word8] ByteString where
  getField = pack

instance HasField "unpack" ByteString [Word8] where
  getField = unpack

instance HasField "take" ByteString (Int -> ByteString) where
  getField = flip take

instance HasField "takeWhile" ByteString ((Word8 -> Bool) -> ByteString) where
  getField = flip takeWhile

instance HasField "takeWhileEnd" ByteString ((Word8 -> Bool) -> ByteString) where
  getField = flip takeWhileEnd

instance HasField "drop" ByteString (Int -> ByteString) where
  getField = flip drop

instance HasField "dropWhile" ByteString ((Word8 -> Bool) -> ByteString) where
  getField = flip dropWhile

instance HasField "dropWhileEnd" ByteString ((Word8 -> Bool) -> ByteString) where
  getField = flip dropWhileEnd

instance HasField "splitAt" ByteString (Int -> (ByteString, ByteString)) where
  getField = flip splitAt

instance HasField "words" ByteString [ByteString] where
  getField = words

instance HasField "unwords" [ByteString] ByteString where
  getField = unwords

instance HasField "lines" ByteString [ByteString] where
  getField = lines

instance HasField "unlines" [ByteString] ByteString where
  getField = unlines

instance HasField "strip" ByteString ByteString where
  getField = strip

instance HasField "inits" ByteString [ByteString] where
  getField = inits

instance HasField "contains" ByteString (ByteString -> Bool) where
  getField = flip contains

instance HasField "stripPrefix" ByteString (ByteString -> Maybe ByteString) where
  getField = flip stripPrefix

instance HasField "stripSuffix" ByteString (ByteString -> Maybe ByteString) where
  getField = flip stripSuffix

instance HasField "asFilePath" ByteString FilePath where
  getField = asFilePath

instance (HasField "read" ByteString (Maybe a), Read a)
       => HasField "read" ByteString (Maybe a) where
  getField = read

instance (HasField "read\7433" ByteString a, Read a)
       => HasField "read\7433" ByteString a where
  getField = StackTrace.suppressForMethod "ByteString.read!" ByteString.read!
