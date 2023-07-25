{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module FilePath (
  FilePath(..)
, (</>)
, (<.>)

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
) where

import Solid.Common hiding (IsString(..))
import Solid.Common qualified as Solid
import Solid.ToString qualified as Solid
import Solid.Types hiding (asByteString)
import Solid.Foreign.C qualified as C
import Data.Coerce (coerce)
import Data.ByteString.Short (fromShort)
import System.OsPath qualified as Haskell
import System.OsString.Internal.Types (OsString(..), PosixString(..))
import System.Directory.OsPath qualified as Haskell
import System.Posix.Files.PosixString qualified as Posix
import System.Posix.Directory.PosixPath qualified as Posix

(</>) :: FilePath -> FilePath -> FilePath
(</>) = coerce (Haskell.</>)

(<.>) :: FilePath -> FilePath -> FilePath
(<.>) = coerce (Haskell.<.>)

toString :: FilePath -> String
toString = decodeUtf8 . asByteString

fromString :: String -> FilePath
fromString = asFilePath

asByteString :: FilePath -> ByteString
asByteString = Bytes . fromShort . getPosixString . getOsString . unFilePath

instance Solid.ToString FilePath where
  toString = toString

instance Solid.IsString FilePath where
  fromString = fromString . String.pack

instance HasField "asByteString" FilePath ByteString where
  getField = asByteString

exists? :: FilePath -> IO Bool
exists? = coerce Haskell.doesPathExist

file? :: FilePath -> IO Bool
file? = coerce Haskell.doesFileExist

directory? :: FilePath -> IO Bool
directory? = coerce Haskell.doesDirectoryExist

absolute :: FilePath -> IO FilePath
absolute = coerce Haskell.makeAbsolute

remove :: FilePath -> IO ()
remove path =
  C.withFilePath path $ C.throwErrnoPathIfMinus1Retry_ "remove" path . c_remove

foreign import ccall unsafe "remove"
  c_remove :: C.String -> IO C.Int

remove! :: FilePath -> IO ()
remove! = coerce Haskell.removePathForcibly

unlink :: FilePath -> IO ()
unlink = coerce Posix.removeLink

rmdir :: FilePath -> IO ()
rmdir = coerce Posix.removeDirectory

rename :: FilePath -> FilePath -> IO ()
rename = coerce Haskell.renamePath

open :: IO.Mode -> FilePath -> IO Handle
open = flip IO.open

instance HasField "toString" FilePath String where
  getField = toString

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
