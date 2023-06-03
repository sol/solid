module Solid.Foreign.C (
  CInt(..)
, CString
, withFilePath
, throwErrnoPathIfMinus1Retry_
) where

import Solid.Common
import Solid.Types
import String
import Solid.Foreign.Haskell (asPlatformPath)

import Foreign.C
import System.Posix.PosixPath.FilePath qualified as Haskell

withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath = Haskell.withFilePath . asPlatformPath

throwErrnoPathIfMinus1Retry_ :: (Eq a, Num a) => String -> FilePath -> IO a -> IO ()
throwErrnoPathIfMinus1Retry_ name = Haskell.throwErrnoPathIfMinus1Retry_ (unpack name) . asPlatformPath
