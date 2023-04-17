{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module FilePath (
  FilePath(..)
, (</>)

, exists?
, file?
, directory?

, remove
, unlink
, rmdir

, rename
) where

import           Solid.Common
import           String
import           Solid.ToString
import           Data.Coerce

import qualified System.FilePath as Haskell
import qualified System.Directory as Haskell

import qualified System.Posix.Files as Posix
import qualified System.Posix.Directory as Posix

import           Foreign.C
import           System.Posix.Internals (withFilePath)
import           System.Posix.Error (throwErrnoPathIfMinus1Retry_)

newtype FilePath = FilePath { unFilePath :: Haskell.FilePath }
  deriving newtype (Eq, Ord, Show, IsString, ToString)

(</>) :: FilePath -> FilePath -> FilePath
(</>) = coerce (Haskell.</>)

exists? :: FilePath -> IO Bool
exists? = coerce Haskell.doesPathExist

file? :: FilePath -> IO Bool
file? = coerce Haskell.doesFileExist

directory? :: FilePath -> IO Bool
directory? = coerce Haskell.doesDirectoryExist

remove :: FilePath -> IO ()
remove (FilePath path) =
  withFilePath path $ throwErrnoPathIfMinus1Retry_ "remove" path . c_remove

foreign import ccall unsafe "remove"
  c_remove :: CString -> IO CInt

unlink :: FilePath -> IO ()
unlink = coerce Posix.removeLink

rmdir :: FilePath -> IO ()
rmdir = coerce Posix.removeDirectory

rename :: FilePath -> FilePath -> IO ()
rename = coerce Haskell.renamePath

instance HasField "toFilePath" [Char] FilePath where
  getField = FilePath

instance HasField "toFilePath" String FilePath where
  getField = FilePath . unpack

instance HasField "toString" FilePath String where
  getField = toString

instance HasField "exists\660" FilePath (IO Bool) where
  getField = exists?

instance HasField "file\660" FilePath (IO Bool) where
  getField = file?

instance HasField "directory\660" FilePath (IO Bool) where
  getField = directory?

instance HasField "remove" FilePath (IO ()) where
  getField = remove

instance HasField "unlink" FilePath (IO ()) where
  getField = unlink

instance HasField "rmdir" FilePath (IO ()) where
  getField = rmdir

instance HasField "rename" FilePath (FilePath -> IO ()) where
  getField = rename
