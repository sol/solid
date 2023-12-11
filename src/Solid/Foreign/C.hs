{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE PatternSynonyms #-}
module Solid.Foreign.C (
  Int
, pattern CInt

, Char
, pattern CChar

, WChar
, pattern CWchar
, toWChar
, fromWChar
, fromWChar!

, String
, StringLen
, WString
, WStringLen

, withCString
, withCStringLen

, withBytesAsCString
, withBytesAsCStringLen

, withFilePath
, throwErrnoPathIfMinus1Retry_
) where

import Solid.Common hiding (Int, Char)
import Solid.Common qualified as Solid
import Solid.String hiding (String)
import Solid.Bytes.Unsafe
import String qualified as Solid

import Foreign.C hiding (
    withCString
  , withCStringLen
  )
import Foreign.Ptr
import System.Posix.PosixPath.FilePath qualified as Posix

import Haskell (asPlatformPath)

use Haskell
use Data.Text.Foreign as Text

type Int = CInt
type Char = CChar

type WChar = CWchar

toWChar :: Solid.Char -> WChar
toWChar = toEnum . fromEnum

fromWChar :: WChar -> Solid.Char
fromWChar c
  | c < unicodeMin = '\xFFFD'
  | c > unicodeMax = '\xFFFD'
  | otherwise = fromWChar! c
  where
    unicodeMin :: WChar
    unicodeMin = toWChar minBound

    unicodeMax :: WChar
    unicodeMax = toWChar maxBound

fromWChar! :: WChar -> Solid.Char
fromWChar! = toEnum . fromEnum

type String = Ptr Char
type StringLen = (String, Solid.Int)
type WString = Ptr WChar
type WStringLen = (WString, Solid.Int)

withCString :: Solid.String -> (String -> IO a) -> IO a
withCString = withBytesAsCString

withCStringLen :: Solid.String -> (StringLen -> IO a) -> IO a
withCStringLen = withBytesAsCStringLen

withBytesAsCString :: Bytes c -> (String -> IO a) -> IO a
withBytesAsCString = Text.withCString . Haskell.unsafeToText

withBytesAsCStringLen :: Bytes c -> (StringLen -> IO a) -> IO a
withBytesAsCStringLen = Text.withCStringLen . Haskell.unsafeToText

withFilePath :: FilePath -> (String -> IO a) -> IO a
withFilePath = Posix.withFilePath . asPlatformPath

throwErrnoPathIfMinus1Retry_ :: (Eq a, Num a) => Solid.String -> FilePath -> IO a -> IO ()
throwErrnoPathIfMinus1Retry_ name = Posix.throwErrnoPathIfMinus1Retry_ (unpack name) . asPlatformPath
