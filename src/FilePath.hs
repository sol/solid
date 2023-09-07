{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module FilePath (
  FilePath(..)
, (Import.</>)
, (Import.<.>)

, toString
, fromString
, asByteString

, exists?
, file?
, directory?

, absolute

, remove
, remove!
, unlink
, rmdir

, rename

, open

, directory
) where

import Solid.Common hiding (IsString(..))
import Solid.FilePath
import Solid.ToString qualified as Solid
import Solid.Bytes.Unsafe
import Solid.Foreign.C qualified as C
import Data.Coerce (coerce)
import System.OsString.Internal.Types (OsString(..), PosixString(..))
import System.Posix.Files.PosixString qualified as Posix
import System.Posix.Directory.PosixPath qualified as Posix

use System.FilePath.Import
use System.Directory.Import

instance Solid.ToString FilePath where
  toString = toString

exists? :: FilePath -> IO Bool
exists? = Import.doesPathExist

file? :: FilePath -> IO Bool
file? = Import.doesFileExist

directory? :: FilePath -> IO Bool
directory? = Import.doesDirectoryExist

absolute :: FilePath -> IO FilePath
absolute = Import.makeAbsolute

remove :: FilePath -> IO ()
remove path =
  C.withFilePath path $ C.throwErrnoPathIfMinus1Retry_ "remove" path . c_remove

foreign import ccall unsafe "remove"
  c_remove :: C.String -> IO C.Int

remove! :: FilePath -> IO ()
remove! = Import.removePathForcibly

unlink :: FilePath -> IO ()
unlink = coerce Posix.removeLink

rmdir :: FilePath -> IO ()
rmdir = coerce Posix.removeDirectory

rename :: FilePath -> FilePath -> IO ()
rename = Import.renamePath

open :: IO.Mode -> FilePath -> IO Handle
open = flip IO.open

directory :: FilePath -> FilePath
directory = Import.takeDirectory

instance HasField "exists\660" FilePath (IO Bool) where
  getField = exists?

instance HasField "file\660" FilePath (IO Bool) where
  getField = file?

instance HasField "directory\660" FilePath (IO Bool) where
  getField = directory?

instance HasField "absolute" FilePath (IO FilePath) where
  getField = absolute

instance HasField "remove" FilePath (IO ()) where
  getField = remove

instance HasField "remove\7433" FilePath (IO ()) where
  getField = remove!

instance HasField "unlink" FilePath (IO ()) where
  getField = unlink

instance HasField "rmdir" FilePath (IO ()) where
  getField = rmdir

instance HasField "rename" FilePath (FilePath -> IO ()) where
  getField = rename

instance HasField "open" FilePath (IO.Mode -> IO Handle) where
  getField = IO.open

instance HasField "directory" FilePath FilePath where
  getField = directory
