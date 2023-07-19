module Data.Text.Import where

import Solid.Common
import Solid.Types

import Data.List qualified as List

import Data.Coerce (coerce)
import Solid.Foreign.Haskell (fromText, toText)

import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Import qualified as Bytes

type Text = String

all :: (Char -> Bool) -> String -> Bool
{-# INLINE all #-}
all p = Text.all p . toText

any :: (Char -> Bool) -> String -> Bool
{-# INLINE any #-}
any p = Text.any p . toText

break :: (Char -> Bool) -> String -> (String, String)
{-# INLINE break #-}
break p = bimap fromText fromText . Text.break p . toText

breakOn :: WithStackTrace => String -> String -> (String, String)
{-# INLINE breakOn #-}
breakOn xs = bimap fromText fromText . Text.breakOn (toText xs) . toText

breakOnAll :: WithStackTrace => String -> String -> [(String, String)]
{-# INLINE breakOnAll #-}
breakOnAll xs = List.map (bimap fromText fromText) . Text.breakOnAll (toText xs) . toText

breakOnEnd :: WithStackTrace => String -> String -> (String, String)
{-# INLINE breakOnEnd #-}
breakOnEnd xs = bimap fromText fromText . Text.breakOnEnd (toText xs) . toText

center :: Int -> Char -> String -> String
{-# INLINE center #-}
center n c = fromText . Text.center n c . toText

chunksOf :: Int -> String -> [String]
{-# INLINE chunksOf #-}
chunksOf n = List.map fromText . Text.chunksOf n . toText

commonPrefixes :: String -> String -> Maybe (String, String, String)
{-# INLINE commonPrefixes #-}
commonPrefixes xs ys = case Text.commonPrefixes (toText xs) (toText ys) of
  Nothing -> Nothing
  Just (as, bs, cs) -> Just (fromText as, fromText bs, fromText cs)

compareLength :: String -> Int -> Ordering
{-# INLINE compareLength #-}
compareLength = Text.compareLength . toText

concat :: [String] -> String
{-# INLINE concat #-}
concat = coerce ByteString.concat

concatMap :: (Char -> String) -> String -> String
{-# INLINE concatMap #-}
concatMap f = fromText . Text.concatMap (toText . f) . toText

cons :: Char -> String -> String
{-# INLINE cons #-}
cons c = fromText . Text.cons c . toText

copy :: String -> String
{-# INLINE copy #-}
copy = Bytes.copy

count :: WithStackTrace => String -> String -> Int
{-# INLINE count #-}
count xs = Text.count (toText xs) . toText

drop :: Int -> String -> String
{-# INLINE drop #-}
drop n = fromText . Text.drop n . toText

dropAround :: (Char -> Bool) -> String -> String
{-# INLINE dropAround #-}
dropAround p = fromText . Text.dropAround p . toText

dropEnd :: Int -> String -> String
{-# INLINE dropEnd #-}
dropEnd n = fromText . Text.dropEnd n . toText

dropWhile :: (Char -> Bool) -> String -> String
{-# INLINE dropWhile #-}
dropWhile p = fromText . Text.dropWhile p . toText

dropWhileEnd :: (Char -> Bool) -> String -> String
{-# INLINE dropWhileEnd #-}
dropWhileEnd p = fromText . Text.dropWhileEnd p . toText

elem :: Char -> String -> Bool
{-# INLINE elem #-}
elem c = Text.elem c . toText

filter :: (Char -> Bool) -> String -> String
{-# INLINE filter #-}
filter p = fromText . Text.filter p . toText

find :: (Char -> Bool) -> String -> Maybe Char
{-# INLINE find #-}
find p = Text.find p . toText

findIndex :: (Char -> Bool) -> String -> Maybe Int
{-# INLINE findIndex #-}
findIndex p = Text.findIndex p . toText

foldl :: (a -> Char -> a) -> a -> String -> a
{-# INLINE foldl #-}
foldl f a = Text.foldl f a . toText

foldl' :: (a -> Char -> a) -> a -> String -> a
{-# INLINE foldl' #-}
foldl' f a = Text.foldl' f a . toText

foldl1 :: WithStackTrace => (Char -> Char -> Char) -> String -> Char
{-# INLINE foldl1 #-}
foldl1 f = Text.foldl1 f . toText

foldl1' :: WithStackTrace => (Char -> Char -> Char) -> String -> Char
{-# INLINE foldl1' #-}
foldl1' f = Text.foldl1' f . toText

foldr :: (Char -> a -> a) -> a -> String -> a
{-# INLINE foldr #-}
foldr f a = Text.foldr f a . toText

foldr' :: (Char -> a -> a) -> a -> String -> a
{-# INLINE foldr' #-}
foldr' f a = Text.foldr' f a . toText

foldr1 :: WithStackTrace => (Char -> Char -> Char) -> String -> Char
{-# INLINE foldr1 #-}
foldr1 f = Text.foldr1 f . toText

group :: String -> [String]
{-# INLINE group #-}
group = List.map fromText . Text.group . toText

groupBy :: (Char -> Char -> Bool) -> String -> [String]
{-# INLINE groupBy #-}
groupBy p = List.map fromText . Text.groupBy p . toText

head :: WithStackTrace => String -> Char
{-# INLINE head #-}
head = Text.head . toText

index :: WithStackTrace => String -> Int -> Char
{-# INLINE index #-}
index = Text.index . toText

init :: WithStackTrace => String -> String
{-# INLINE init #-}
init = fromText . Text.init . toText

inits :: String -> [String]
{-# INLINE inits #-}
inits = List.map fromText . Text.inits . toText

intercalate :: String -> [String] -> String
{-# INLINE intercalate #-}
intercalate = coerce ByteString.intercalate

intersperse :: Char -> String -> String
{-# INLINE intersperse #-}
intersperse c = fromText . Text.intersperse c . toText

isAscii :: String -> Bool
{-# INLINE isAscii #-}
isAscii = Bytes.all (< 128)

isInfixOf :: String -> String -> Bool
{-# INLINE isInfixOf #-}
isInfixOf = Bytes.isInfixOf

isPrefixOf :: String -> String -> Bool
{-# INLINE isPrefixOf #-}
isPrefixOf = Bytes.isPrefixOf

isSuffixOf :: String -> String -> Bool
{-# INLINE isSuffixOf #-}
isSuffixOf = Bytes.isSuffixOf

justifyLeft :: Int -> Char -> String -> String
{-# INLINE justifyLeft #-}
justifyLeft n c = fromText . Text.justifyLeft n c . toText

justifyRight :: Int -> Char -> String -> String
{-# INLINE justifyRight #-}
justifyRight n c = fromText . Text.justifyRight n c . toText

last :: WithStackTrace => String -> Char
{-# INLINE last #-}
last = Text.last . toText

length :: String -> Int
{-# INLINE length #-}
length = Text.length . toText

lines :: String -> [String]
{-# INLINE lines #-}
lines = coerce Char8.lines

map :: (Char -> Char) -> String -> String
{-# INLINE map #-}
map f = fromText . Text.map f . toText

mapAccumL :: (a -> Char -> (a, Char)) -> a -> String -> (a, String)
{-# INLINE mapAccumL #-}
mapAccumL f a = fmap fromText . Text.mapAccumL f a . toText

mapAccumR :: (a -> Char -> (a, Char)) -> a -> String -> (a, String)
{-# INLINE mapAccumR #-}
mapAccumR f a = fmap fromText . Text.mapAccumR f a . toText

maximum :: WithStackTrace => String -> Char
{-# INLINE maximum #-}
maximum = Text.maximum . toText

measureOff :: Int -> String -> Int
{-# INLINE measureOff #-}
measureOff n = Text.measureOff n . toText

minimum :: WithStackTrace => String -> Char
{-# INLINE minimum #-}
minimum = Text.minimum . toText

null :: String -> Bool
{-# INLINE null #-}
null = Bytes.null

partition :: (Char -> Bool) -> String -> (String, String)
{-# INLINE partition #-}
partition p = bimap fromText fromText . Text.partition p . toText

replace :: WithStackTrace => String -> String -> String -> String
{-# INLINE replace #-}
replace xs ys = fromText . Text.replace (toText xs) (toText ys) . toText

replicate :: Int -> String -> String
{-# INLINE replicate #-}
replicate n = fromText . Text.replicate n . toText

reverse :: String -> String
{-# INLINE reverse #-}
reverse = fromText . Text.reverse . toText

scanl :: (Char -> Char -> Char) -> Char -> String -> String
{-# INLINE scanl #-}
scanl f c = fromText . Text.scanl f c . toText

scanl1 :: (Char -> Char -> Char) -> String -> String
{-# INLINE scanl1 #-}
scanl1 f = fromText . Text.scanl1 f . toText

scanr :: (Char -> Char -> Char) -> Char -> String -> String
{-# INLINE scanr #-}
scanr f c = fromText . Text.scanr f c . toText

scanr1 :: (Char -> Char -> Char) -> String -> String
{-# INLINE scanr1 #-}
scanr1 f = fromText . Text.scanr1 f . toText

snoc :: String -> Char -> String
{-# INLINE snoc #-}
snoc xs = fromText . Text.snoc (toText xs)

span :: (Char -> Bool) -> String -> (String, String)
{-# INLINE span #-}
span p = bimap fromText fromText . Text.span p . toText

spanEndM :: Monad m => (Char -> m Bool) -> String -> m (String, String)
{-# INLINE spanEndM #-}
spanEndM p = fmap (bimap fromText fromText) . Text.spanEndM p . toText

spanM :: Monad m => (Char -> m Bool) -> String -> m (String, String)
{-# INLINE spanM #-}
spanM p = fmap (bimap fromText fromText) . Text.spanM p . toText

split :: (Char -> Bool) -> String -> [String]
{-# INLINE split #-}
split p = List.map fromText . Text.split p . toText

splitAt :: Int -> String -> (String, String)
{-# INLINE splitAt #-}
splitAt n = bimap fromText fromText . Text.splitAt n . toText

splitOn :: WithStackTrace => String -> String -> [String]
{-# INLINE splitOn #-}
splitOn xs = List.map fromText . Text.splitOn (toText xs) . toText

strip :: String -> String
{-# INLINE strip #-}
strip = fromText . Text.strip . toText

stripEnd :: String -> String
{-# INLINE stripEnd #-}
stripEnd = fromText . Text.stripEnd . toText

stripPrefix :: String -> String -> Maybe String
{-# INLINE stripPrefix #-}
stripPrefix = coerce ByteString.stripPrefix

stripStart :: String -> String
{-# INLINE stripStart #-}
stripStart = fromText . Text.stripStart . toText

stripSuffix :: String -> String -> Maybe String
{-# INLINE stripSuffix #-}
stripSuffix = coerce ByteString.stripSuffix

tail :: WithStackTrace => String -> String
{-# INLINE tail #-}
tail = fromText . Text.tail . toText

tails :: String -> [String]
{-# INLINE tails #-}
tails = List.map fromText . Text.tails . toText

take :: Int -> String -> String
{-# INLINE take #-}
take n = fromText . Text.take n . toText

takeEnd :: Int -> String -> String
{-# INLINE takeEnd #-}
takeEnd n = fromText . Text.takeEnd n . toText

takeWhile :: (Char -> Bool) -> String -> String
{-# INLINE takeWhile #-}
takeWhile p = fromText . Text.takeWhile p . toText

takeWhileEnd :: (Char -> Bool) -> String -> String
{-# INLINE takeWhileEnd #-}
takeWhileEnd p = fromText . Text.takeWhileEnd p . toText

toCaseFold :: String -> String
{-# INLINE toCaseFold #-}
toCaseFold = fromText . Text.toCaseFold . toText

toLower :: String -> String
{-# INLINE toLower #-}
toLower = fromText . Text.toLower . toText

toTitle :: String -> String
{-# INLINE toTitle #-}
toTitle = fromText . Text.toTitle . toText

toUpper :: String -> String
{-# INLINE toUpper #-}
toUpper = fromText . Text.toUpper . toText

transpose :: [String] -> [String]
{-# INLINE transpose #-}
transpose = List.map fromText . Text.transpose . List.map toText

uncons :: String -> Maybe (Char, String)
{-# INLINE uncons #-}
uncons = fmap (fmap fromText) . Text.uncons . toText

unfoldr :: (a -> Maybe (Char, a)) -> a -> String
{-# INLINE unfoldr #-}
unfoldr f = fromText . Text.unfoldr f

unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> String
{-# INLINE unfoldrN #-}
unfoldrN n f = fromText . Text.unfoldrN n f

unlines :: [String] -> String
{-# INLINE unlines #-}
unlines = coerce Char8.unlines

unsnoc :: String -> Maybe (String, Char)
{-# INLINE unsnoc #-}
unsnoc = fmap (first fromText) . Text.unsnoc . toText

unwords :: [String] -> String
{-# INLINE unwords #-}
unwords = coerce Char8.unwords

words :: String -> [String]
{-# INLINE words #-}
words = List.map fromText . Text.words . toText

zip :: String -> String -> [(Char, Char)]
{-# INLINE zip #-}
zip xs ys = Text.zip (toText xs) (toText ys)

zipWith :: (Char -> Char -> Char) -> String -> String -> String
{-# INLINE zipWith #-}
zipWith f xs ys = fromText $ Text.zipWith f (toText xs) (toText ys)

append :: String -> String -> String
{-# INLINE append #-}
append = coerce ByteString.append

empty :: String
{-# INLINE empty #-}
empty = coerce ByteString.empty

pack :: [Char] -> String
{-# INLINE pack #-}
pack = Bytes . Text.encodeUtf8 . Text.pack

singleton :: Char -> String
{-# INLINE singleton #-}
singleton = fromText . Text.singleton

unpack :: String -> [Char]
{-# INLINE unpack #-}
unpack = Text.unpack . Text.decodeUtf8 . unBytes
