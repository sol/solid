{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
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

-- * Others
, times
, replicate
, copy
, compact
) where

import Solid.Common hiding (empty, take, drop, last, tail, init, null, head, splitAt, concat, replicate, map, reverse, foldr, foldr1, foldl, foldl1, concatMap, any, all, maximum, minimum)

import HaskellPrelude (error)
import GHC.Stack
import Data.Semigroup
import Data.List.NonEmpty (NonEmpty)

import GHC.Exts
import GHC.Show (intToDigit)
import Data.Bits ((.&.), unsafeShiftR)
use Data.List
use Data.Text
use Data.Text.Array
use Simd.Utf8

import Data.Sliced.ByteArray.Util
import Data.Sliced.ByteArray.Unsafe
import Data.Sliced.ByteArray.Conversion

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

unsafeDrop :: Int -> ByteArray -> ByteArray
unsafeDrop n (ByteArray arr off len) = ByteArray arr (off + n) (len - n)
{-# INLINE unsafeDrop #-}

unsafeDropEnd  :: Int -> ByteArray -> ByteArray
unsafeDropEnd n (ByteArray arr off len) = ByteArray arr off (len - n)
{-# INLINE unsafeDropEnd #-}

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
