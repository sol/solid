module System.Directory.Import (
  Permissions(..)
, XdgDirectory(..)
, XdgDirectoryList(..)
, module System.Directory.Import
) where

import Solid.Common
import Solid.Bytes.Unsafe
import Data.Coerce (coerce)

import System.Directory.OsPath (Permissions(..), XdgDirectory(..), XdgDirectoryList(..))
import System.Directory.OsPath qualified as Haskell
import Data.Time.Clock (UTCTime)

canonicalizePath :: FilePath -> IO FilePath
canonicalizePath = coerce Haskell.canonicalizePath

copyFile :: FilePath -> FilePath -> IO ()
copyFile = coerce Haskell.copyFile

copyFileWithMetadata :: FilePath -> FilePath -> IO ()
copyFileWithMetadata = coerce Haskell.copyFileWithMetadata

copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions = coerce Haskell.copyPermissions

createDirectory :: FilePath -> IO ()
createDirectory = coerce Haskell.createDirectory

createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing = coerce Haskell.createDirectoryIfMissing

createDirectoryLink :: FilePath -> FilePath -> IO ()
createDirectoryLink = coerce Haskell.createDirectoryLink

createFileLink :: FilePath -> FilePath -> IO ()
createFileLink = coerce Haskell.createFileLink

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist = coerce Haskell.doesDirectoryExist

doesFileExist :: FilePath -> IO Bool
doesFileExist = coerce Haskell.doesFileExist

doesPathExist :: FilePath -> IO Bool
doesPathExist = coerce Haskell.doesPathExist

emptyPermissions :: Permissions
emptyPermissions = coerce Haskell.emptyPermissions

exeExtension :: FilePath
exeExtension = coerce Haskell.exeExtension

findExecutable :: FilePath -> IO (Maybe FilePath)
findExecutable = coerce Haskell.findExecutable

findExecutables :: FilePath -> IO [FilePath]
findExecutables = coerce Haskell.findExecutables

findExecutablesInDirectories :: [FilePath] -> FilePath -> IO [FilePath]
findExecutablesInDirectories = coerce Haskell.findExecutablesInDirectories

findFile :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findFile = coerce Haskell.findFile

findFileWith :: (FilePath -> IO Bool) -> [FilePath] -> FilePath -> IO (Maybe FilePath)
findFileWith = coerce Haskell.findFileWith

findFiles :: [FilePath] -> FilePath -> IO [FilePath]
findFiles = coerce Haskell.findFiles

findFilesWith :: (FilePath -> IO Bool) -> [FilePath] -> FilePath -> IO [FilePath]
findFilesWith = coerce Haskell.findFilesWith

getAccessTime :: FilePath -> IO UTCTime
getAccessTime = coerce Haskell.getAccessTime

getAppUserDataDirectory :: FilePath -> IO FilePath
getAppUserDataDirectory = coerce Haskell.getAppUserDataDirectory

getCurrentDirectory :: IO FilePath
getCurrentDirectory = coerce Haskell.getCurrentDirectory

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents = coerce Haskell.getDirectoryContents

getFileSize :: FilePath -> IO Integer
getFileSize = coerce Haskell.getFileSize

getHomeDirectory :: IO FilePath
getHomeDirectory = coerce Haskell.getHomeDirectory

getModificationTime :: FilePath -> IO UTCTime
getModificationTime = coerce Haskell.getModificationTime

getPermissions :: FilePath -> IO Permissions
getPermissions = coerce Haskell.getPermissions

getSymbolicLinkTarget :: FilePath -> IO FilePath
getSymbolicLinkTarget = coerce Haskell.getSymbolicLinkTarget

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = coerce Haskell.getTemporaryDirectory

getUserDocumentsDirectory :: IO FilePath
getUserDocumentsDirectory = coerce Haskell.getUserDocumentsDirectory

getXdgDirectory :: XdgDirectory -> FilePath -> IO FilePath
getXdgDirectory = coerce Haskell.getXdgDirectory

getXdgDirectoryList :: XdgDirectoryList -> IO [FilePath]
getXdgDirectoryList = coerce Haskell.getXdgDirectoryList

listDirectory :: FilePath -> IO [FilePath]
listDirectory = coerce Haskell.listDirectory

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute = coerce Haskell.makeAbsolute

makeRelativeToCurrentDirectory :: FilePath -> IO FilePath
makeRelativeToCurrentDirectory = coerce Haskell.makeRelativeToCurrentDirectory

pathIsSymbolicLink :: FilePath -> IO Bool
pathIsSymbolicLink = coerce Haskell.pathIsSymbolicLink

removeDirectory :: FilePath -> IO ()
removeDirectory = coerce Haskell.removeDirectory

removeDirectoryLink :: FilePath -> IO ()
removeDirectoryLink = coerce Haskell.removeDirectoryLink

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive = coerce Haskell.removeDirectoryRecursive

removeFile :: FilePath -> IO ()
removeFile = coerce Haskell.removeFile

removePathForcibly :: FilePath -> IO ()
removePathForcibly = coerce Haskell.removePathForcibly

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory = coerce Haskell.renameDirectory

renameFile :: FilePath -> FilePath -> IO ()
renameFile = coerce Haskell.renameFile

renamePath :: FilePath -> FilePath -> IO ()
renamePath = coerce Haskell.renamePath

setAccessTime :: FilePath -> UTCTime -> IO ()
setAccessTime = coerce Haskell.setAccessTime

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory = coerce Haskell.setCurrentDirectory

setModificationTime :: FilePath -> UTCTime -> IO ()
setModificationTime = coerce Haskell.setModificationTime

setOwnerExecutable :: Bool -> Permissions -> Permissions
setOwnerExecutable = coerce Haskell.setOwnerExecutable

setOwnerReadable :: Bool -> Permissions -> Permissions
setOwnerReadable = coerce Haskell.setOwnerReadable

setOwnerSearchable :: Bool -> Permissions -> Permissions
setOwnerSearchable = coerce Haskell.setOwnerSearchable

setOwnerWritable :: Bool -> Permissions -> Permissions
setOwnerWritable = coerce Haskell.setOwnerWritable

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions = coerce Haskell.setPermissions

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory = Haskell.withCurrentDirectory . coerce
