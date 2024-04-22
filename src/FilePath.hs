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

, parent
, directory
) where

import Solid.Common hiding (IsString(..))
import Solid.FilePath
import Solid.ToString qualified as Solid
import Solid.Bytes.Unsafe
import Data.Coerce (coerce)
import Solid.OsString (OsString(..), PosixString(..))
import System.Posix.Files.PosixString qualified as Posix
import System.Posix.Directory.PosixPath qualified as Posix

use Solid.Foreign.C
use System.FilePath.Import
use System.Directory.Import

instance Solid.ToString FilePath where
  toString = toString

.exists? :: FilePath -> IO Bool
.exists? = Import.doesPathExist

.file? :: FilePath -> IO Bool
.file? = Import.doesFileExist

.directory? :: FilePath -> IO Bool
.directory? = Import.doesDirectoryExist

.absolute :: FilePath -> IO FilePath
.absolute = Import.makeAbsolute

.remove :: FilePath -> IO ()
.remove path =
  C.withFilePath path $ C.throwErrnoPathIfMinus1Retry_ "remove" path . c_remove

foreign import ccall unsafe "remove"
  c_remove :: C.String -> IO C.Int

.remove! :: FilePath -> IO ()
.remove! = Import.removePathForcibly

.unlink :: FilePath -> IO ()
.unlink = coerce Posix.removeLink

.rmdir :: FilePath -> IO ()
.rmdir = coerce Posix.removeDirectory

rename :: FilePath -> FilePath -> IO ()
rename = Import.renamePath

.open :: IO.Mode -> FilePath -> IO Handle
.open = flip IO.open

.parent :: FilePath -> FilePath
.parent = Import.takeDirectory . Import.dropTrailingPathSeparator

.directory :: FilePath -> FilePath
.directory = Import.takeDirectory

instance HasField "rename" FilePath (FilePath -> IO ()) where
  getField = rename
