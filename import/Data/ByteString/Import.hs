module Data.ByteString.Import (
  ByteString
, module Data.ByteString.Import
) where

import Solid.Common
import Solid.Bytes
import Solid.Bytes.Unsafe
import Solid.ByteString (ByteString)
import FilePath qualified

import Data.Coerce (coerce)
import Haskell qualified

import Foreign.C (CString, CStringLen)
import Data.List.NonEmpty (NonEmpty)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified

type StrictByteString = ByteString

(!?) :: Bytes enc -> Int -> Maybe Word8
{-# INLINE (!?) #-}
(!?) = coerce (ByteString.!?)

all :: (Word8 -> Bool) -> Bytes enc -> Bool
{-# INLINE all #-}
all = coerce ByteString.all

any :: (Word8 -> Bool) -> Bytes enc -> Bool
{-# INLINE any #-}
any = coerce ByteString.any

append :: ByteString -> ByteString -> ByteString
{-# INLINE append #-}
append = coerce ByteString.append

appendFile :: FilePath -> Bytes enc -> IO ()
{-# INLINE appendFile #-}
appendFile path (Bytes content) = Haskell.toFilePath path >>= (`ByteString.appendFile` content)

break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
{-# INLINE break #-}
break = coerce ByteString.break

breakEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
{-# INLINE breakEnd #-}
breakEnd = coerce ByteString.breakEnd

breakSubstring :: ByteString -> ByteString -> (ByteString, ByteString)
{-# INLINE breakSubstring #-}
breakSubstring = coerce ByteString.breakSubstring

concat :: [ByteString] -> ByteString
{-# INLINE concat #-}
concat = coerce ByteString.concat

concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
{-# INLINE concatMap #-}
concatMap = coerce ByteString.concatMap

cons :: Word8 -> ByteString -> ByteString
{-# INLINE cons #-}
cons = coerce ByteString.cons

copy :: Bytes enc -> Bytes enc
{-# INLINE copy #-}
copy = coerce ByteString.copy

count :: Word8 -> Bytes enc -> Int
{-# INLINE count #-}
count = coerce ByteString.count

drop :: Int -> ByteString -> ByteString
{-# INLINE drop #-}
drop = coerce ByteString.drop

dropEnd :: Int -> ByteString -> ByteString
{-# INLINE dropEnd #-}
dropEnd = coerce ByteString.dropEnd

dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
{-# INLINE dropWhile #-}
dropWhile = coerce ByteString.dropWhile

dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
{-# INLINE dropWhileEnd #-}
dropWhileEnd = coerce ByteString.dropWhileEnd

elem :: Word8 -> Bytes enc -> Bool
{-# INLINE elem #-}
elem = coerce ByteString.elem

elemIndex :: Word8 -> Bytes enc -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex = coerce ByteString.elemIndex

elemIndexEnd :: Word8 -> Bytes enc -> Maybe Int
{-# INLINE elemIndexEnd #-}
elemIndexEnd = coerce ByteString.elemIndexEnd

elemIndices :: Word8 -> Bytes enc -> [Int]
{-# INLINE elemIndices #-}
elemIndices = coerce ByteString.elemIndices

filter :: (Word8 -> Bool) -> ByteString -> ByteString
{-# INLINE filter #-}
filter = coerce ByteString.filter

find :: (Word8 -> Bool) -> Bytes enc -> Maybe Word8
{-# INLINE find #-}
find = coerce ByteString.find

findIndex :: (Word8 -> Bool) -> Bytes enc -> Maybe Int
{-# INLINE findIndex #-}
findIndex = coerce ByteString.findIndex

findIndexEnd :: (Word8 -> Bool) -> Bytes enc -> Maybe Int
{-# INLINE findIndexEnd #-}
findIndexEnd = coerce ByteString.findIndexEnd

findIndices :: (Word8 -> Bool) -> Bytes enc -> [Int]
{-# INLINE findIndices #-}
findIndices = coerce ByteString.findIndices

foldl :: (a -> Word8 -> a) -> a -> Bytes enc -> a
{-# INLINE foldl #-}
foldl = coerce . ByteString.foldl

foldl' :: (a -> Word8 -> a) -> a -> Bytes enc -> a
{-# INLINE foldl' #-}
foldl' = coerce . ByteString.foldl'

foldl1 :: WithStackTrace => (Word8 -> Word8 -> Word8) -> Bytes enc -> Word8
{-# INLINE foldl1 #-}
foldl1 = coerce ByteString.foldl1

foldl1' :: WithStackTrace => (Word8 -> Word8 -> Word8) -> Bytes enc -> Word8
{-# INLINE foldl1' #-}
foldl1' = coerce ByteString.foldl1'

foldr :: (Word8 -> a -> a) -> a -> Bytes enc -> a
{-# INLINE foldr #-}
foldr = coerce . ByteString.foldr

foldr' :: (Word8 -> a -> a) -> a -> Bytes enc -> a
{-# INLINE foldr' #-}
foldr' = coerce . ByteString.foldr'

foldr1 :: WithStackTrace => (Word8 -> Word8 -> Word8) -> Bytes enc -> Word8
{-# INLINE foldr1 #-}
foldr1 = coerce ByteString.foldr1

foldr1' :: WithStackTrace => (Word8 -> Word8 -> Word8) -> Bytes enc -> Word8
{-# INLINE foldr1' #-}
foldr1' = coerce ByteString.foldr1'

fromFilePath :: FilePath -> IO ByteString
{-# INLINE fromFilePath #-}
fromFilePath = return . FilePath.asByteString

getContents :: IO ByteString
{-# INLINE getContents #-}
getContents = coerce ByteString.getContents

getLine :: IO ByteString
{-# INLINE getLine #-}
getLine = coerce Char8.getLine

group :: ByteString -> [ByteString]
{-# INLINE group #-}
group = coerce ByteString.group

groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
{-# INLINE groupBy #-}
groupBy = coerce ByteString.groupBy

hGet :: Handle -> Int -> IO ByteString
{-# INLINE hGet #-}
hGet = coerce ByteString.hGet

hGetContents :: Handle -> IO ByteString
{-# INLINE hGetContents #-}
hGetContents = coerce ByteString.hGetContents

hGetLine :: Handle -> IO ByteString
{-# INLINE hGetLine #-}
hGetLine = coerce Char8.hGetLine

hGetNonBlocking :: Handle -> Int -> IO ByteString
{-# INLINE hGetNonBlocking #-}
hGetNonBlocking = coerce ByteString.hGetNonBlocking

hGetSome :: Handle -> Int -> IO ByteString
{-# INLINE hGetSome #-}
hGetSome = coerce ByteString.hGetSome

hPut :: Handle -> Bytes enc -> IO ()
{-# INLINE hPut #-}
hPut = coerce ByteString.hPut

hPutNonBlocking :: Handle -> ByteString -> IO ByteString
{-# INLINE hPutNonBlocking #-}
hPutNonBlocking = coerce ByteString.hPutNonBlocking

hPutStr :: Handle -> Bytes enc -> IO ()
{-# INLINE hPutStr #-}
hPutStr = coerce ByteString.hPutStr

head :: WithStackTrace => Bytes enc -> Word8
{-# INLINE head #-}
head = coerce ByteString.head

index :: WithStackTrace => Bytes enc -> Int -> Word8
{-# INLINE index #-}
index = coerce ByteString.index

indexMaybe :: Bytes enc -> Int -> Maybe Word8
{-# INLINE indexMaybe #-}
indexMaybe = coerce ByteString.indexMaybe

init :: WithStackTrace => ByteString -> ByteString
{-# INLINE init #-}
init = coerce ByteString.init

inits :: ByteString -> [ByteString]
{-# INLINE inits #-}
inits = coerce ByteString.inits

initsNE :: ByteString -> NonEmpty ByteString
{-# INLINE initsNE #-}
initsNE = coerce ByteString.initsNE

interact :: (Bytes enc -> Bytes enc) -> IO ()
{-# INLINE interact #-}
interact = coerce ByteString.interact

intercalate :: ByteString -> [ByteString] -> ByteString
{-# INLINE intercalate #-}
intercalate = coerce ByteString.intercalate

intersperse :: Word8 -> ByteString -> ByteString
{-# INLINE intersperse #-}
intersperse = coerce ByteString.intersperse

isInfixOf :: Bytes enc -> Bytes enc -> Bool
{-# INLINE isInfixOf #-}
isInfixOf = coerce ByteString.isInfixOf

isPrefixOf :: Bytes enc -> Bytes enc -> Bool
{-# INLINE isPrefixOf #-}
isPrefixOf = coerce ByteString.isPrefixOf

isSuffixOf :: Bytes enc -> Bytes enc -> Bool
{-# INLINE isSuffixOf #-}
isSuffixOf = coerce ByteString.isSuffixOf

isValidUtf8 :: Bytes enc -> Bool
{-# INLINE isValidUtf8 #-}
isValidUtf8 = coerce ByteString.isValidUtf8

last :: WithStackTrace => Bytes enc -> Word8
{-# INLINE last #-}
last = coerce ByteString.last

length :: Bytes enc -> Int
{-# INLINE length #-}
length = coerce ByteString.length

map :: (Word8 -> Word8) -> ByteString -> ByteString
{-# INLINE map #-}
map = coerce ByteString.map

mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
{-# INLINE mapAccumL #-}
mapAccumL = coerce . ByteString.mapAccumL

mapAccumR :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
{-# INLINE mapAccumR #-}
mapAccumR = coerce . ByteString.mapAccumR

maximum :: WithStackTrace => Bytes enc -> Word8
{-# INLINE maximum #-}
maximum = coerce ByteString.maximum

minimum :: WithStackTrace => Bytes enc -> Word8
{-# INLINE minimum #-}
minimum = coerce ByteString.minimum

notElem :: Word8 -> Bytes enc -> Bool
{-# INLINE notElem #-}
notElem = coerce ByteString.notElem

null :: Bytes enc -> Bool
{-# INLINE null #-}
null = coerce ByteString.null

pack :: [Word8] -> ByteString
{-# INLINE pack #-}
pack = coerce ByteString.pack

packCString :: CString -> IO ByteString
{-# INLINE packCString #-}
packCString = coerce ByteString.packCString

packCStringLen :: CStringLen -> IO ByteString
{-# INLINE packCStringLen #-}
packCStringLen = coerce ByteString.packCStringLen

packZipWith :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString -> ByteString
{-# INLINE packZipWith #-}
packZipWith = coerce ByteString.packZipWith

partition :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
{-# INLINE partition #-}
partition = coerce ByteString.partition

putStr :: Bytes enc -> IO ()
{-# INLINE putStr #-}
putStr = coerce ByteString.putStr

readFile :: FilePath -> IO ByteString
{-# INLINE readFile #-}
readFile = Haskell.toFilePath >=> fmap Bytes . ByteString.readFile

replicate :: Int -> Word8 -> ByteString
{-# INLINE replicate #-}
replicate = coerce ByteString.replicate

reverse :: ByteString -> ByteString
{-# INLINE reverse #-}
reverse = coerce ByteString.reverse

scanl :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
{-# INLINE scanl #-}
scanl = coerce ByteString.scanl

scanl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
{-# INLINE scanl1 #-}
scanl1 = coerce ByteString.scanl1

scanr :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteString -> ByteString
{-# INLINE scanr #-}
scanr = coerce ByteString.scanr

scanr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
{-# INLINE scanr1 #-}
scanr1 = coerce ByteString.scanr1

singleton :: Word8 -> ByteString
{-# INLINE singleton #-}
singleton = coerce ByteString.singleton

snoc :: ByteString -> Word8 -> ByteString
{-# INLINE snoc #-}
snoc = coerce ByteString.snoc

sort :: ByteString -> ByteString
{-# INLINE sort #-}
sort = coerce ByteString.sort

span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
{-# INLINE span #-}
span = coerce ByteString.span

spanEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
{-# INLINE spanEnd #-}
spanEnd = coerce ByteString.spanEnd

split :: Word8 -> ByteString -> [ByteString]
{-# INLINE split #-}
split = coerce ByteString.split

splitAt :: Int -> ByteString -> (ByteString, ByteString)
{-# INLINE splitAt #-}
splitAt = coerce ByteString.splitAt

splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
{-# INLINE splitWith #-}
splitWith = coerce ByteString.splitWith

stripPrefix :: ByteString -> ByteString -> Maybe ByteString
{-# INLINE stripPrefix #-}
stripPrefix = coerce ByteString.stripPrefix

stripSuffix :: ByteString -> ByteString -> Maybe ByteString
{-# INLINE stripSuffix #-}
stripSuffix = coerce ByteString.stripSuffix

tail :: WithStackTrace => ByteString -> ByteString
{-# INLINE tail #-}
tail = coerce ByteString.tail

tails :: ByteString -> [ByteString]
{-# INLINE tails #-}
tails = coerce ByteString.tails

tailsNE :: ByteString -> NonEmpty ByteString
{-# INLINE tailsNE #-}
tailsNE = coerce ByteString.tailsNE

take :: Int -> ByteString -> ByteString
{-# INLINE take #-}
take = coerce ByteString.take

takeEnd :: Int -> ByteString -> ByteString
{-# INLINE takeEnd #-}
takeEnd = coerce ByteString.takeEnd

takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
{-# INLINE takeWhile #-}
takeWhile = coerce ByteString.takeWhile

takeWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
{-# INLINE takeWhileEnd #-}
takeWhileEnd = coerce ByteString.takeWhileEnd

toFilePath :: Bytes enc -> IO FilePath
{-# INLINE toFilePath #-}
toFilePath = return . asFilePath

transpose :: [ByteString] -> [ByteString]
{-# INLINE transpose #-}
transpose = coerce ByteString.transpose

uncons :: ByteString -> Maybe (Word8, ByteString)
{-# INLINE uncons #-}
uncons = coerce ByteString.uncons

unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString
{-# INLINE unfoldr #-}
unfoldr f = coerce . ByteString.unfoldr f

unfoldrN :: Int -> (a -> Maybe (Word8, a)) -> a -> (ByteString, Maybe a)
{-# INLINE unfoldrN #-}
unfoldrN n f = coerce . ByteString.unfoldrN n f

unpack :: Bytes enc -> [Word8]
{-# INLINE unpack #-}
unpack = coerce ByteString.unpack

unsnoc :: ByteString -> Maybe (ByteString, Word8)
{-# INLINE unsnoc #-}
unsnoc = coerce ByteString.unsnoc

unzip :: [(Word8, Word8)] -> (ByteString, ByteString)
{-# INLINE unzip #-}
unzip = coerce ByteString.unzip

useAsCString :: Bytes enc -> (CString -> IO a) -> IO a
{-# INLINE useAsCString #-}
useAsCString = ByteString.useAsCString . coerce

useAsCStringLen :: Bytes enc -> (CStringLen -> IO a) -> IO a
{-# INLINE useAsCStringLen #-}
useAsCStringLen = ByteString.useAsCStringLen . coerce

writeFile :: FilePath -> Bytes enc -> IO ()
{-# INLINE writeFile #-}
writeFile path (Bytes content) = Haskell.toFilePath path >>= (`ByteString.writeFile` content)

zip :: Bytes enc -> Bytes enc -> [(Word8, Word8)]
{-# INLINE zip #-}
zip = coerce ByteString.zip

zipWith :: (Word8 -> Word8 -> a) -> Bytes enc -> Bytes enc -> [a]
{-# INLINE zipWith #-}
zipWith = coerce . ByteString.zipWith

empty :: ByteString
{-# INLINE empty #-}
empty = coerce ByteString.empty

fromStrict :: ByteString -> Data.ByteString.Lazy.ByteString
{-# INLINE fromStrict #-}
fromStrict = coerce ByteString.fromStrict

toStrict :: Data.ByteString.Lazy.ByteString -> ByteString
{-# INLINE toStrict #-}
toStrict = coerce ByteString.toStrict
