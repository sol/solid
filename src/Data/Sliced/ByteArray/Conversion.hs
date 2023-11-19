{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Sliced.ByteArray.Conversion (
  toByteString
, fromByteString

, toShortByteString
, fromShortByteString

, toLazyByteString
, fromLazyByteString

, unsafeToText
, fromText

, toOsPath
, fromOsPath
) where

import Solid.Common

import Data.Sliced.ByteArray.Util
import Data.Sliced.ByteArray.Unsafe

import Control.Monad.ST.Unsafe (unsafeSTToIO, unsafeIOToST)
import GHC.ForeignPtr (unsafeWithForeignPtr)
import Data.Primitive.ByteArray (byteArrayAsForeignPtr)
use Data.Text.Array

import Data.ByteString.Internal (ByteString(..))
use Data.ByteString

import Data.ByteString.Short (ShortByteString(..), toShort)
use Data.ByteString.Short as ShortByteString

import Data.ByteString.Lazy (LazyByteString)
use Data.ByteString.Lazy.Internal as Lazy

import Data.Text.Internal (Text(..))

import System.OsPath (OsPath)
import System.OsString.Internal.Types (OsString(..), PosixString(..))

toByteString :: ByteArray -> ByteString
toByteString = asByteString . pin
  where
    asByteString :: ByteArray -> ByteString
    asByteString bytes = PS (byteArrayAsForeignPtr bytes.arr) bytes.off bytes.len
{-# INLINE toByteString #-}

fromByteString :: ByteString -> ByteArray
fromByteString = fromShortByteString . toShort
{-# INLINE fromByteString #-}

toShortByteString :: ByteArray -> ShortByteString
toShortByteString bytes = ShortByteString (compact bytes).arr
{-# INLINE toShortByteString #-}

fromShortByteString :: ShortByteString -> ByteArray
fromShortByteString bytestring@(ShortByteString arr) = ByteArray{..}
  where
    off = 0
    len = ShortByteString.length bytestring
{-# INLINE fromShortByteString #-}

toLazyByteString :: ByteArray -> LazyByteString
toLazyByteString = Lazy.fromStrict . toByteString
{-# INLINE toLazyByteString #-}

fromLazyByteString :: LazyByteString -> ByteArray
fromLazyByteString input = ByteArray arr 0 len
  where
    len = lazyByteStringLength input
    arr = create len $ \ marr -> do
      copyChunks marr 0 input

lazyByteStringLength :: LazyByteString -> Int
lazyByteStringLength = Lazy.foldlChunks addLength 0
  where
    addLength :: Int -> ByteString -> Int
    addLength n c = checkedAdd "Conversion.fromLazyByteString" n (ByteString.length c)
{-# INLINE lazyByteStringLength #-}

copyChunks :: MArray s -> Int -> LazyByteString -> ST s ()
copyChunks marr off = \ case
  Lazy.Empty -> return ()
  Lazy.Chunk (BS src len) chunks -> do
    unsafeIOToST $ unsafeWithForeignPtr src $ \ ptr -> do
      unsafeSTToIO $ Array.copyFromPointer marr off ptr len
    copyChunks marr (off + len) chunks

unsafeToText :: ByteArray -> Text
unsafeToText ByteArray{..} = Text arr off len
{-# INLINE unsafeToText #-}

fromText :: Text -> ByteArray
fromText (Text arr off len) = ByteArray{..}
{-# INLINE fromText #-}

toOsPath :: ByteArray -> OsPath
toOsPath = OsString . PosixString . toShortByteString
{-# INLINE toOsPath #-}

fromOsPath :: OsPath -> ByteArray
fromOsPath = fromShortByteString . getPosixString . getOsString
{-# INLINE fromOsPath #-}
