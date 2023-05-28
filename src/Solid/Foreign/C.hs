module Solid.Foreign.C (
  CInt(..)
, CString
, withFilePath
, throwErrnoPathIfMinus1Retry_
) where

import Solid.Common
import Solid.Types
import String

import Foreign.C
import System.OsString.Internal.Types (OsString(..), PlatformString)
import System.Posix.PosixPath.FilePath qualified as Haskell

asPlatformString :: FilePath -> PlatformString
asPlatformString = getOsString . unFilePath

withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath = Haskell.withFilePath . asPlatformString

throwErrnoPathIfMinus1Retry_ :: (Eq a, Num a) => String -> FilePath -> IO a -> IO ()
throwErrnoPathIfMinus1Retry_ name = Haskell.throwErrnoPathIfMinus1Retry_ (unpack name) . asPlatformString
