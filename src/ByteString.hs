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
use Data.Sliced.ByteArray

import Data.Coerce (coerce)
import Data.ByteString.Internal (isSpaceWord8)

-- $setup
-- >>> import Solid

instance Show ByteString where
  showsPrec n = showsPrec n . unBytes

instance IsString ByteString where
  fromString = Bytes.asByteString . String.pack

.asString :: ByteString -> Maybe String
.asString = String.asString

.asString! :: WithStackTrace => ByteString -> String
.asString! (Bytes string) = if ByteArray.isValidUtf8 string then Bytes string else throw! UnicodeDecodeError

.decodeUtf8 :: ByteString -> String
.decodeUtf8 = String.decodeUtf8

empty :: ByteString
empty = mempty

.empty? :: ByteString -> Bool
.empty? = coerce ByteArray.null

null :: ByteString -> Bool
null = coerce ByteArray.null

.length :: ByteString -> Int
.length = coerce ByteArray.length

.pack :: [Word8] -> ByteString
.pack = Bytes . ByteArray.pack

.unpack :: ByteString -> [Word8]
.unpack = ByteArray.unpack . unBytes

-- |
-- >>> ByteString.take 2 "foobar"
-- "fo"
.take :: Int -> ByteString -> ByteString
.take = coerce ByteArray.take

-- |
-- >>> ByteString.drop 2 "foobar"
-- "obar"
.drop :: Int -> ByteString -> ByteString
.drop = coerce ByteArray.drop

-- |
-- >>> ByteString.takeWhile (/= fromIntegral 'b'.ord) "foobar"
-- "foo"
.takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
.takeWhile = coerce ByteArray.takeWhile

-- |
-- >>> ByteString.dropWhile (/= fromIntegral 'b'.ord) "foobar"
-- "bar"
.dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
.dropWhile = coerce ByteArray.dropWhile

-- |
-- >>> ByteString.takeWhileEnd (/= fromIntegral 'b'.ord) "foobar"
-- "ar"
.takeWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
.takeWhileEnd = coerce ByteArray.takeWhileEnd

-- |
-- >>> ByteString.dropWhileEnd (/= fromIntegral 'b'.ord) "foobar"
-- "foob"
.dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
.dropWhileEnd = coerce ByteArray.dropWhileEnd

-- |
-- >>> ByteString.splitAt 3 "foobar"
-- ("foo","bar")
.splitAt :: Int -> ByteString -> (ByteString, ByteString)
.splitAt = coerce ByteArray.splitAt

-- |
-- >>> ByteString.slice 3 5 "foobar"
-- "ba"
.slice :: Int -> Int -> ByteString -> ByteString
.slice = coerce ByteArray.slice

-- |
-- >>> ByteString.chunksOf 2 "foobarbaz"
-- ["fo","ob","ar","ba","z"]
.chunksOf :: Int -> ByteString -> [ByteString]
.chunksOf = coerce ByteArray.chunksOf

-- |
-- >>> ByteString.breakOn "ba" "foobarbaz"
-- ("foo","barbaz")
.breakOn :: ByteString -> ByteString -> (ByteString, ByteString)
.breakOn = coerce ByteArray.breakOn

-- |
-- >>> ByteString.unwords ["foo", "bar", "baz"]
-- "foo bar baz"
.unwords :: [ByteString] -> ByteString
.unwords = coerce ByteArray.unwords

.lines :: ByteString -> [ByteString]
.lines = coerce ByteArray.lines

.unlines :: [ByteString] -> ByteString
.unlines = coerce ByteArray.unlines

-- |
-- >>> let input = "hey-there" :: ByteString
--
-- >>> input.split "-"
-- ["hey","there"]
--
-- >>> input.split ""
-- ["h","e","y","-","t","h","e","r","e"]
.split :: ByteString -> ByteString -> [ByteString]
.split = coerce ByteArray.split

.strip :: ByteString -> ByteString
.strip = dropWhile asciiSpace? . dropWhileEnd asciiSpace?
  where
    asciiSpace? c = c < 128 && isSpaceWord8 c

.inits :: ByteString -> [ByteString]
.inits = coerce ByteArray.inits

isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf = Bytes.isPrefixOf

isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf = Bytes.isSuffixOf

isInfixOf :: ByteString -> ByteString -> Bool
isInfixOf = coerce ByteArray.isInfixOf

.stripPrefix :: ByteString -> ByteString -> Maybe ByteString
.stripPrefix = coerce ByteArray.stripPrefix

.stripSuffix :: ByteString -> ByteString -> Maybe ByteString
.stripSuffix = coerce ByteArray.stripSuffix

startsWith :: ByteString -> ByteString -> Bool
startsWith = Bytes.startsWith

endsWith :: ByteString -> ByteString -> Bool
endsWith = Bytes.endsWith

.contains :: ByteString -> ByteString -> Bool
.contains = isInfixOf

.asFilePath :: ByteString -> FilePath
.asFilePath = Bytes.asFilePath

read :: Read a => ByteString -> Maybe a
read = asString >=> String.read

read! :: WithStackTrace => Read a => ByteString -> a
read! input = case read input of
  Nothing -> StackTrace.suppress Exception.invalidValue! "no parse"
  Just a -> a

instance (HasField "read" ByteString (Maybe a), Read a)
       => HasField "read" ByteString (Maybe a) where
  getField = read

instance (HasField "read\7433" ByteString a, Read a)
       => HasField "read\7433" ByteString a where
  getField = StackTrace.suppressForMethod "ByteString.read!" ByteString.read!
