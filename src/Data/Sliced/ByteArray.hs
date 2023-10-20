{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Data.Sliced.ByteArray (
  ByteArray

-- * Creation and elimination
, pack
, unpack
, singleton
, empty

-- * Basic interface
, cons
, snoc
, append
, uncons
, unsnoc
, head
, last
, tail
, init
, null
, length

-- * Encoding validation
, isAscii
, isValidUtf8

-- * Transformations
, map
, reverse
, intersperse
, intercalate

-- * Folds
, foldl
, foldl'
, foldl1
, foldl1'

, foldr
, foldr'
, foldr1
, foldr1'

-- ** Special folds
, concat
, any
, all

-- * Substrings
-- ** Breaking strings
, take
, drop
, splitAt

, takeWhile
, dropWhile
, span
, break

, takeWhileEnd
, dropWhileEnd
, spanEnd
, breakEnd

, stripPrefix
, stripSuffix

-- ** Breaking into many substrings
, split

-- ** Breaking into lines and words
, lines
, unlines
, unwords

-- * Predicates
, isPrefixOf
, isSuffixOf
, isInfixOf

-- * Searching
, elem

-- * Others
, times
, replicate
, copy
, compact

, strip

, empty?
, inits
, hGetContents
, hPut
, readFile
, writeFile


, errorEmpty -- FIXME: remove

, hGetLine
) where

import Solid.Common hiding (empty, take, drop, last, tail, init, null, head, splitAt, concat, replicate, map, reverse, foldr, foldr1, foldl, foldl1, concatMap, any, all, maximum, minimum, takeWhile, dropWhile, break, span, elem)

import HaskellPrelude (error)
import System.IO (FilePath)
import GHC.Stack
import Data.Semigroup
import Data.List.NonEmpty (NonEmpty)

import GHC.Exts
import GHC.Show (intToDigit)
import Data.Bits ((.&.), unsafeShiftR)
import Foreign.C.Types
import System.Posix.Types (CSsize(..))
use Data.List
use Data.Text
use Data.Text.Array
use Data.Text.Internal.Search
use Simd.Utf8

import Data.Sliced.ByteArray.Util
import Data.Sliced.ByteArray.Unsafe
import Data.Sliced.ByteArray.Conversion

use Data.ByteString.Char8
use Data.ByteString

use Data.Bytes as ByteSlice

instance Show ByteArray where
  showsPrec :: Int -> ByteArray -> ShowS
  showsPrec n bytes
    | isValidUtf8 bytes = showsPrec n (unsafeToText bytes)
    | otherwise = showAsList bytes

showAsList :: ByteArray -> ShowS
showAsList bytes = showChar '[' . go 0
  where
    go i
      | i < bytes.len = comma . showWord8 (unsafeIndex i bytes) . go i.succ
      | otherwise = showChar ']'
      where
        comma
          | i == 0 = id
          | otherwise = showString ", "

    showWord8 :: Word8 -> [Char] -> [Char]
    showWord8 !c rest = '0' : 'x' : hi : lo : rest
      where
        toDigit :: Word8 -> Char
        toDigit = intToDigit . fromIntegral

        hi = toDigit (unsafeShiftR c 4)
        lo = toDigit (c .&. 0x0F)

instance IsString ByteArray where
  fromString = fromText . Text.pack

instance IsList ByteArray where
  type Item ByteArray = Word8
  fromListN n xs = ByteArray (fromListN n xs) 0 n
  fromList xs = fromListN (List.length xs) xs
  toList = unpack

instance Semigroup ByteArray where
  (<>) = append

  sconcat :: NonEmpty ByteArray -> ByteArray
  sconcat = concat . toList

  stimes :: Integral n => n -> ByteArray -> ByteArray
  stimes (toInteger -> n) bytes
    | n <= (0 :: Integer) || bytes.len <= 0 = empty
    | toInteger nInt == n = times nInt bytes
    | otherwise = overflowError "stimes"
    where
      nInt :: Int
      nInt = fromInteger n

instance Monoid ByteArray where
  mempty = empty
  mconcat = concat

pack :: [Word8] -> ByteArray
pack = fromList

unpack :: ByteArray -> [Word8]
unpack bytes = go bytes.off
  where
    !done = bytes.off + bytes.len

    go :: Int -> [Word8]
    go !off = if off < done
      then Array.unsafeIndex bytes.arr off : go off.succ
      else []

singleton :: Word8 -> ByteArray
singleton c = ByteArray arr 0 1
  where
    arr :: Array
    arr = create 1 $ \ marr -> do
      Array.unsafeWrite marr 0 c

append :: ByteArray -> ByteArray -> ByteArray
append a b
  | a.len == 0 = b
  | b.len == 0 = a
  | otherwise = ByteArray arr 0 len
  where
    len = checkedAdd "append" a.len b.len
    arr = create len $ \ marr -> do
      copyTo marr 0 a
      copyTo marr a.len b

concat :: [ByteArray] -> ByteArray
concat (discardEmpty -> xs) = case xs of
  [] -> empty
  [t] -> t
  _ -> ByteArray arr 0 len
  where
    len = checkedSum "concat" $ List.map (.len) xs
    arr = create len $ \ marr -> do
      foldM_ (\ off bytes -> do
        copyTo marr off bytes
        return (off + bytes.len)
        ) 0 xs

discardEmpty :: [ByteArray] -> [ByteArray]
discardEmpty = List.filter (\ x -> x.len > 0)
{-# INLINE discardEmpty #-}

times :: Int -> ByteArray -> ByteArray
times n bytes
  | n <= 0 || bytes.len <= 0 = empty
  | n == 1 = bytes
  | bytes.len == 1 = unsafeReplicate n (unsafeHead bytes)
  | otherwise = ByteArray arr 0 len
  where
    len = checkedMultiply "times" n bytes.len
    arr = create len $ \ marr -> do
      copyTo marr 0 bytes
      Array.tile marr bytes.len

replicate :: Int -> Word8 -> ByteArray
replicate n c
  | n <= 0 = empty
  | otherwise = unsafeReplicate n c

unsafeReplicate :: Int -> Word8 -> ByteArray
unsafeReplicate !n !c = runST $ do
  marr <- Array.newFilled n (fromIntegral c)
  arr <- Array.unsafeFreeze marr
  return $ ByteArray arr 0 n

infixr 5 `cons`
infixl 5 `snoc`

cons :: Word8 -> ByteArray -> ByteArray
cons x xs = ByteArray {..}
  where
    off = 0
    len = succ xs.len
    arr = create len $ \ marr -> do
      copyTo marr 1 xs
      Array.unsafeWrite marr 0 x

snoc :: ByteArray -> Word8 -> ByteArray
snoc xs x = ByteArray {..}
  where
    off = 0
    len = succ xs.len
    arr = create len $ \ marr -> do
      copyTo marr 0 xs
      Array.unsafeWrite marr xs.len x

uncons :: ByteArray -> Maybe (Word8, ByteArray)
uncons = nothingOnEmpty $ \ bytes -> (unsafeHead bytes, unsafeTail bytes)

unsnoc :: ByteArray -> Maybe (ByteArray, Word8)
unsnoc = nothingOnEmpty $ \ bytes -> (unsafeInit bytes, unsafeLast bytes)

head :: HasCallStack => ByteArray -> Word8
head = errorOnEmpty unsafeHead

last :: HasCallStack => ByteArray -> Word8
last = errorOnEmpty unsafeLast

tail :: HasCallStack => ByteArray -> ByteArray
tail = errorOnEmpty unsafeTail

init :: HasCallStack => ByteArray -> ByteArray
init = errorOnEmpty unsafeInit

null :: ByteArray -> Bool
null bytes = bytes.len == 0
{-# INLINE null #-}

length :: ByteArray -> Int
length bytes = bytes.len
{-# INLINE length #-}

map :: (Word8 -> Word8) -> ByteArray -> ByteArray
map f bytes = ByteArray arr 0 bytes.len
  where
    arr :: Array
    arr = create bytes.len $ \ marr -> do
      let
        go i
          | i < bytes.len = do
              Array.unsafeWrite marr i (f $ unsafeIndex i bytes)
              go i.succ
          | otherwise = pass
      go 0

reverse :: ByteArray -> ByteArray
reverse bytes = ByteArray arr 0 bytes.len
  where
    arr :: Array
    arr = create bytes.len $ \ marr -> do
      let
        go from to
          | to < bytes.len = do
              Array.unsafeWrite marr to (unsafeIndex from bytes)
              go from.pred to.succ
          | otherwise = pass
      go (bytes.len - 1) 0

intersperse :: Word8 -> ByteArray -> ByteArray
intersperse c bytes
  | bytes.len < 2  = bytes
  | otherwise = ByteArray arr 0 len
  where
    len = 2 * bytes.len - 1
    arr = runST $ do
      marr <- Array.newFilled len (fromIntegral c)
      let
        go i
          | i < bytes.len = do
              Array.unsafeWrite marr (i * 2) (unsafeIndex i bytes)
              go i.succ
          | otherwise = pass
      go 0
      Array.unsafeFreeze marr

intercalate :: ByteArray -> [ByteArray] -> ByteArray
intercalate _ [] = mempty
intercalate _ [chunk] = chunk
intercalate sep (firstChunk : chunks) = ByteArray arr 0 len
  where
    plus = checkedAdd "intercalate"
    len = List.foldl' (\ acc chunk -> acc `plus` sep.len `plus` chunk.len) firstChunk.len chunks
    arr = create len $ \ marr -> do
      copyTo marr 0 firstChunk
      let
        go _ [] = pure ()
        go i (x : xs) = do
          copyTo marr i sep
          let j = i + sep.len
          copyTo marr j x
          go (j + x.len) xs
      go firstChunk.len chunks

foldl :: (a -> Word8 -> a) -> a -> ByteArray -> a
foldl f start = \ case -- the lambda is crucial as GHC only inlines functions that are "fully applied"
  bytes -> go bytes.len.pred
    where
      go !i
        | i < 0 = start
        | otherwise = let !x = unsafeIndex i bytes in f (go i.pred) x
{-# INLINE foldl #-}

foldr :: (Word8 -> a -> a) -> a -> ByteArray -> a
foldr f start = \ case -- the lambda is crucial as GHC only inlines functions that are "fully applied"
  bytes -> go 0
    where
      go !i
        | i < bytes.len = let !x = unsafeIndex i bytes in f x (go i.succ)
        | otherwise = start
{-# INLINE foldr #-}

foldl' :: (a -> Word8 -> a) -> a -> ByteArray -> a
foldl' f start = \ case -- the lambda is crucial as GHC only inlines functions that are "fully applied"
  bytes -> go start 0
    where
      go !acc !i
        | i < bytes.len = let !x = unsafeIndex i bytes in go (f acc x) i.succ
        | otherwise = acc
{-# INLINE foldl' #-}

foldr' :: (Word8 -> a -> a) -> a -> ByteArray -> a
foldr' f start = \ case -- the lambda is crucial as GHC only inlines functions that are "fully applied"
  bytes -> go start bytes.len.pred
    where
      go !acc !i
        | i < 0 = acc
        | otherwise = let !x = unsafeIndex i bytes in go (f x acc) i.pred
{-# INLINE foldr' #-}

foldl1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteArray -> Word8
foldl1 f = errorOnEmpty $ \ bytes -> foldl f (unsafeHead bytes) (unsafeTail bytes)
{-# INLINE foldl1 #-}

foldl1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteArray -> Word8
foldl1' f = errorOnEmpty $ \ bytes -> foldl' f (unsafeHead bytes) (unsafeTail bytes)
{-# INLINE foldl1' #-}

foldr1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteArray -> Word8
foldr1 f = errorOnEmpty $ \ bytes -> foldr f (unsafeLast bytes) (unsafeInit bytes)
{-# INLINE foldr1 #-}

foldr1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteArray -> Word8
foldr1' f = errorOnEmpty $ \ bytes -> foldr' f (unsafeLast bytes) (unsafeInit bytes)
{-# INLINE foldr1' #-}

any :: (Word8 -> Bool) -> ByteArray -> Bool
any p bytes = go 0
  where
    go i
      | i < bytes.len = if p (unsafeIndex i bytes) then True else go i.succ
      | otherwise = False

all :: (Word8 -> Bool) -> ByteArray -> Bool
all p bytes = go 0
  where
    go i
      | i < bytes.len = if p (unsafeIndex i bytes) then go i.succ else False
      | otherwise = True

isAscii :: ByteArray -> Bool
isAscii = Text.isAscii . unsafeToText

isValidUtf8 :: ByteArray -> Bool
isValidUtf8 ByteArray{..} = Utf8.isValid arr off len

take :: Int -> ByteArray -> ByteArray
take i bytes
  | n >= bytes.len = bytes
  | n == 0 = empty
  | i > 0 = unsafeTake n bytes
  | otherwise = unsafeTakeEnd n bytes
  where
    n = abs i
{-# INLINE take #-}

drop  :: Int -> ByteArray -> ByteArray
drop i bytes
  | n >= bytes.len = empty
  | n == 0 = bytes
  | i > 0 = unsafeDrop n bytes
  | otherwise = unsafeDropEnd n bytes
  where
    n = abs i
{-# INLINE drop #-}

splitAt :: Int -> ByteArray -> (ByteArray, ByteArray)
splitAt i bytes
  | i == 0 = (empty, bytes)
  | i >= bytes.len  = (bytes, empty)
  | i > 0 = (unsafeTake i bytes, unsafeDrop i bytes)
  | n >= bytes.len = (empty, bytes)
  | otherwise = (unsafeDropEnd n bytes, unsafeTakeEnd n bytes)
  where
    n = abs i
{-# INLINE splitAt #-}

takeWhile :: (Word8 -> Bool) -> ByteArray -> ByteArray
takeWhile p bytes = unsafeTake n bytes
  where
    n = countWhile p bytes
{-# INLINE takeWhile #-}

dropWhile :: (Word8 -> Bool) -> ByteArray -> ByteArray
dropWhile p bytes = unsafeDrop n bytes
  where
    n = countWhile p bytes
{-# INLINE dropWhile #-}

span :: (Word8 -> Bool) -> ByteArray -> (ByteArray, ByteArray)
span p bytes = (unsafeTake n bytes, unsafeDrop n bytes)
  where
    n = countWhile p bytes
{-# INLINE span #-}

break :: (Word8 -> Bool) -> ByteArray -> (ByteArray, ByteArray)
break p = span (not . p)
{-# INLINE break #-}

takeWhileEnd :: (Word8 -> Bool) -> ByteArray -> ByteArray
takeWhileEnd p bytes = unsafeTakeEnd n bytes
  where
    n = countWhileEnd p bytes
{-# INLINE takeWhileEnd #-}

dropWhileEnd :: (Word8 -> Bool) -> ByteArray -> ByteArray
dropWhileEnd p bytes = unsafeDropEnd n bytes
  where
    n = countWhileEnd p bytes
{-# INLINE dropWhileEnd #-}

spanEnd :: (Word8 -> Bool) -> ByteArray -> (ByteArray, ByteArray)
spanEnd p bytes = (unsafeDropEnd n bytes, unsafeTakeEnd n bytes)
  where
    n = countWhileEnd p bytes
{-# INLINE spanEnd #-}

breakEnd :: (Word8 -> Bool) -> ByteArray -> (ByteArray, ByteArray)
breakEnd p = spanEnd (not . p)
{-# INLINE breakEnd #-}

countWhile :: (Word8 -> Bool) -> ByteArray -> Int
countWhile p bytes = go 0
  where
    go i
      | i < bytes.len = if p (unsafeIndex i bytes ) then go i.succ else done
      | otherwise = done
      where
        done = i
{-# INLINE countWhile #-}

countWhileEnd :: (Word8 -> Bool) -> ByteArray -> Int
countWhileEnd p bytes = go start
  where
    start = bytes.len.pred
    go i
      | i >= 0 = if p (unsafeIndex i bytes ) then go i.pred else done
      | otherwise = done
      where
        done = start - i
{-# INLINE countWhileEnd #-}

stripPrefix :: ByteArray -> ByteArray -> Maybe ByteArray
stripPrefix prefix bytes
  | isPrefixOf prefix bytes = Just (unsafeDrop prefix.len bytes)
  | otherwise = Nothing
{-# INLINE stripPrefix #-}

stripSuffix :: ByteArray -> ByteArray -> Maybe ByteArray
stripSuffix suffix bytes
  | isSuffixOf suffix bytes = Just (unsafeDropEnd suffix.len bytes)
  | otherwise = Nothing
{-# INLINE stripSuffix #-}

split :: ByteArray -> ByteArray -> [ByteArray]
split needle bytes
  | bytes.len == 0 = [""]
  | needle.len == 0 = elements bytes
  | otherwise = go 0 $ Search.indices (unsafeToText needle) (unsafeToText bytes)
  where
    go off = \ case
      [] -> [unsafeDrop off bytes]
      n : xs -> unsafeSlice off n bytes : go (n + needle.len) xs

elements :: ByteArray -> [ByteArray]
elements bytes = go 0
  where
    go n
      | n < bytes.len = unsafeSlice n m bytes : go m
      | otherwise = []
      where m = n.succ

isPrefixOf :: ByteArray -> ByteArray -> Bool
isPrefixOf prefix bytes
  | prefix.len == 0 = True
  | prefix.len > bytes.len = False
  | otherwise = Array.equal prefix.arr prefix.off bytes.arr bytes.off prefix.len

isSuffixOf :: ByteArray -> ByteArray -> Bool
isSuffixOf suffix bytes
  | suffix.len == 0 = True
  | suffix.len > bytes.len = False
  | otherwise = Array.equal suffix.arr suffix.off bytes.arr off suffix.len
  where
    off = bytes.off + bytes.len - suffix.len

isInfixOf :: ByteArray -> ByteArray -> Bool
isInfixOf needle haystack
  | needle.len == 0 = True
  | needle.len == 1 = elem (unsafeHead needle) haystack
  | otherwise = not . List.null $ Search.indices (unsafeToText needle) (unsafeToText haystack)

elem :: Word8 -> ByteArray -> Bool
elem c bytes = memchr bytes.arr bytes.off bytes.len c >= 0

memchr :: Array -> Int -> Int -> Word8 -> CSsize
memchr (Array.ByteArray bytes) off len = c_memchr bytes (fromIntegral off) (fromIntegral len)

foreign import ccall unsafe "_hs_text_memchr" c_memchr
  :: ByteArray# -> CSize -> CSize -> Word8 -> CSsize

unsafeIndex :: Int -> ByteArray -> Word8
unsafeIndex n bytes = Array.unsafeIndex bytes.arr (bytes.off + n)
{-# INLINE unsafeIndex #-}

unsafeHead :: ByteArray -> Word8
unsafeHead bytes = Array.unsafeIndex bytes.arr bytes.off
{-# INLINE unsafeHead #-}

unsafeLast :: ByteArray -> Word8
unsafeLast bytes = Array.unsafeIndex bytes.arr (bytes.off + bytes.len - 1)
{-# INLINE unsafeLast #-}

unsafeTail :: ByteArray -> ByteArray
unsafeTail = unsafeDrop 1
{-# INLINE unsafeTail #-}

unsafeInit :: ByteArray -> ByteArray
unsafeInit = unsafeDropEnd 1
{-# INLINE unsafeInit #-}

unsafeTake :: Int -> ByteArray -> ByteArray
unsafeTake n (ByteArray arr off _) = ByteArray arr off n
{-# INLINE unsafeTake #-}

unsafeTakeEnd :: Int -> ByteArray -> ByteArray
unsafeTakeEnd n (ByteArray arr off len) = ByteArray arr (off + len - n) n
{-# INLINE unsafeTakeEnd #-}

unsafeDrop :: Int -> ByteArray -> ByteArray
unsafeDrop n (ByteArray arr off len) = ByteArray arr (off + n) (len - n)
{-# INLINE unsafeDrop #-}

unsafeDropEnd  :: Int -> ByteArray -> ByteArray
unsafeDropEnd n (ByteArray arr off len) = ByteArray arr off (len - n)
{-# INLINE unsafeDropEnd #-}

unsafeSlice :: Int -> Int -> ByteArray -> ByteArray
unsafeSlice start end (ByteArray arr off _) = ByteArray arr (off + start) (end - start)
{-# INLINE unsafeSlice #-}

nothingOnEmpty :: (ByteArray -> a) -> ByteArray -> Maybe a
nothingOnEmpty f = withNonEmpty Nothing (Just . f)
{-# INLINE nothingOnEmpty #-}

errorOnEmpty :: HasCallStack => (ByteArray -> a) -> ByteArray -> a
errorOnEmpty = withNonEmpty $ withCallStack (freezeCallStack . popCallStack) errorEmpty
{-# INLINE errorOnEmpty #-}

errorEmpty :: HasCallStack => a
errorEmpty = error "empty ByteArray"

withNonEmpty :: a -> (ByteArray -> a) -> ByteArray -> a
withNonEmpty a f bytes
  | bytes.len <= 0 = a
  | otherwise = f bytes
{-# INLINE withNonEmpty #-}

lines :: ByteArray -> [ByteArray]
lines = List.map fromText . Text.lines . unsafeToText

unlines :: [ByteArray] -> ByteArray
unlines chunks = ByteArray arr 0 len
  where
    plus = checkedAdd "unlines"
    len = List.foldl' (\ acc chunk -> acc `plus` chunk.len `plus` 1) 0 chunks
    arr = create len $ \ marr -> do
      foldM_ (copyChunk marr) 0 chunks
    copyChunk marr i chunk = do
      copyTo marr i chunk
      let j = i + chunk.len
      Array.unsafeWrite marr j 10
      return j.succ

unwords :: [ByteArray] -> ByteArray
unwords = intercalate " "

-- *************************************************************************
-- *************************************************************************
-- *************************************************************************

empty? :: ByteArray -> Bool
empty? = null
{-# INLINE empty? #-}

instance HasField "emptyʔ" ByteArray Bool where
  getField = empty?

inits :: ByteArray -> [ByteArray]
inits bytes = empty : [unsafeTake n bytes | n <- [1.. bytes.len]]

hGetContents :: Handle -> IO ByteArray
hGetContents = fmap fromByteString . ByteString.hGetContents

hPut :: Handle -> ByteArray -> IO ()
hPut = undefined

readFile :: FilePath -> IO ByteArray
readFile = undefined

writeFile :: FilePath -> ByteArray -> IO ()
writeFile = undefined

hGetLine :: Handle -> IO ByteArray
hGetLine = fmap fromByteString . Char8.hGetLine

{-
(!?) :: ByteArray -> Int -> Maybe Word8
breakSubstring :: ByteArray -> ByteArray -> (ByteArray, ByteArray)
count :: Word8 -> ByteArray -> Int
elemIndex :: Word8 -> ByteArray -> Maybe Int
elemIndexEnd :: Word8 -> ByteArray -> Maybe Int
elemIndices :: Word8 -> ByteArray -> [Int]
filter :: (Word8 -> Bool) -> ByteArray -> ByteArray
find :: (Word8 -> Bool) -> ByteArray -> Maybe Word8
findIndex :: (Word8 -> Bool) -> ByteArray -> Maybe Int
findIndexEnd :: (Word8 -> Bool) -> ByteArray -> Maybe Int
findIndices :: (Word8 -> Bool) -> ByteArray -> [Int]
fromFilePath :: FilePath -> IO ByteArray
getContents :: IO ByteArray
getLine :: IO ByteArray
group :: ByteArray -> [ByteArray]
groupBy :: (Word8 -> Word8 -> Bool) -> ByteArray -> [ByteArray]
hGet :: GHC.IO.Handle.Types.Handle -> Int -> IO ByteArray
hGetNonBlocking :: GHC.IO.Handle.Types.Handle -> Int -> IO ByteArray
hGetSome :: GHC.IO.Handle.Types.Handle -> Int -> IO ByteArray
hPutNonBlocking :: GHC.IO.Handle.Types.Handle -> ByteArray -> IO ByteArray
hPutStr :: GHC.IO.Handle.Types.Handle -> ByteArray -> IO ()
index :: HasCallStack => ByteArray -> Int -> Word8
indexMaybe :: ByteArray -> Int -> Maybe Word8
initsNE :: ByteArray -> GHC.Base.NonEmpty ByteArray
interact :: (ByteArray -> ByteArray) -> IO ()
mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteArray -> (acc, ByteArray)
mapAccumR :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteArray -> (acc, ByteArray)
notElem :: Word8 -> ByteArray -> Bool
packCString :: base:GHC.Foreign.Internal.CString -> IO ByteArray
packCStringLen :: base:GHC.Foreign.Internal.CStringLen -> IO ByteArray
packZipWith :: (Word8 -> Word8 -> Word8) -> ByteArray -> ByteArray -> ByteArray
partition :: (Word8 -> Bool) -> ByteArray -> (ByteArray, ByteArray)
putStr :: ByteArray -> IO ()
scanl :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteArray -> ByteArray
scanl1 :: (Word8 -> Word8 -> Word8) -> ByteArray -> ByteArray
scanr :: (Word8 -> Word8 -> Word8) -> Word8 -> ByteArray -> ByteArray
scanr1 :: (Word8 -> Word8 -> Word8) -> ByteArray -> ByteArray
sort :: ByteArray -> ByteArray
tails :: ByteArray -> [ByteArray]
tailsNE :: ByteArray -> GHC.Base.NonEmpty ByteArray
toFilePath :: ByteArray -> IO FilePath
unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteArray
unfoldrN :: Int -> (a -> Maybe (Word8, a)) -> a -> (ByteArray, Maybe a)
unsnoc :: ByteArray -> Maybe (ByteArray, Word8)
unzip :: [(Word8, Word8)] -> (ByteArray, ByteArray)
useAsCString :: ByteArray -> (base:GHC.Foreign.Internal.CString -> IO a) -> IO a
useAsCStringLen :: ByteArray -> (base:GHC.Foreign.Internal.CStringLen -> IO a) -> IO a
zip :: ByteArray -> ByteArray -> [(Word8, Word8)]
zipWith :: (Word8 -> Word8 -> a) -> ByteArray -> ByteArray -> [a]
type ByteArray :: *
data ByteArray = bytestring:Data.Sliced.ByteArray.Internal.Type.BS {-# UNPACK #-}(GHC.ForeignPtr.ForeignPtr Word8) {-# UNPACK #-}Int
type StrictBytes :: *
type StrictBytes = ByteArray
empty :: ByteArray
fromStrict :: ByteArray -> Data.Sliced.ByteArray.Lazy.Internal.ByteArray
toStrict :: Data.Sliced.ByteArray.Lazy.Internal.ByteArray -> ByteArray
-}

strip :: ByteArray -> ByteArray
strip = undefined
