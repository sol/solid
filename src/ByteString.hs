{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module ByteString (
  ByteString
, module ByteString
) where

import Solid.Common hiding (join, concat, read, head, take, drop, takeWhile, dropWhile, splitAt)
import Solid.String (String)
import Solid.ByteString
import Solid.Bytes.Unsafe
import Exception
use Solid.Bytes
use Solid.String
use String
use Solid.StackTrace
use Data.Sliced.ByteArray
use Data.Sliced.ByteArray.Unsafe as ByteArray

import Data.Coerce (coerce)
import Data.ByteString.Internal (isSpaceWord8)

-- $setup
-- >>> import Solid ()
-- >>> import Test.QuickCheck
-- >>> instance Arbitrary ByteString where arbitrary = pack <$> arbitrary

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

infixr 5 `cons`

.cons :: Word8 -> ByteString -> ByteString
.cons = coerce ByteArray.cons

-- |
-- >>> ByteString.head "foobar"
-- Just 102
.head :: ByteString -> Maybe Word8
.head = coerce ByteArray.safeHead

.head! :: WithStackTrace => ByteString -> Word8
.head! (Bytes bytes)
  | bytes.len == 0 = StackTrace.suppress Exception.invalidValue! "empty ByteString"
  | otherwise = ByteArray.unsafeHead bytes

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

-- |
-- >>> words "foo  bar \n baz"
-- ["foo","bar","baz"]
.words :: ByteString -> [ByteString]
.words = List.filter (not . empty?) . splitWith asciiSpace?

.unlines :: [ByteString] -> ByteString
.unlines = coerce ByteArray.unlines

-- | Join a list of strings.
--
-- >>> ByteString.join "-" ["hey", "there"]
-- "hey-there"
--
-- prop> join sep xs == concat (List.intersperse sep xs)
.join :: ByteString -> [ByteString] -> ByteString
.join = coerce ByteArray.intercalate

.concat :: [ByteString] -> ByteString
.concat = coerce ByteArray.concat

-- | Split the /input/ at every occurrence of a /pattern/.
--
-- >>> let input = "hey-there" :: ByteString
-- >>> input.split "-"
-- ["hey","there"]
--
-- prop> split input input == ["", ""]
-- prop> join pat (split pat input) == input
.split :: ByteString -> ByteString -> [ByteString]
.split = coerce ByteArray.split

-- |
-- >>> splitWith (== 97) "aabbaca"
-- ["","","bb","c",""]
--
-- >>> splitWith undefined ""
-- [""]
.splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
.splitWith = coerce ByteArray.splitWith

-- | Replace every occurrence of a /pattern/ with a /substitute/.
--
-- >>> let message = "I am not angry. Not at all." :: ByteString
-- >>> message.replace "." "!"
-- "I am not angry! Not at all!"
--
-- >>> let hey = "hey" :: ByteString
-- >>> hey.replace "" "-"
-- "-h-e-y-"
--
-- prop> replace pat pat input == input
-- prop> replace input sub input == sub
-- prop> replace pat sub input == (input.split pat).join sub
.replace :: ByteString -> ByteString -> ByteString -> ByteString
.replace = coerce ByteArray.replace

.strip :: ByteString -> ByteString
.strip = dropWhile asciiSpace? . dropWhileEnd asciiSpace?

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

.read :: Read a => ByteString -> Maybe a
.read = asString >=> String.read

.read! :: WithStackTrace => Read a => ByteString -> a
.read! input = case read input of
  Nothing -> StackTrace.suppress Exception.invalidValue! "no parse"
  Just a -> a

asciiSpace? :: Word8 -> Bool
asciiSpace? c = isSpaceWord8 c && c < 128
{-# INLINE asciiSpace? #-}
