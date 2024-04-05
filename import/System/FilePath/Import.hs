{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module System.FilePath.Import (
  FilePath
, module System.FilePath.Import
) where

import Solid.Common
import Solid.Bytes.Unsafe
import Solid.FilePath ()

import Data.Coerce (coerce)
import GHC.IO.Exception
import System.OsPath qualified as OsPath
import System.FilePath qualified as FilePath
use Solid.OsString
import System.Posix.Env.PosixString qualified as Posix

(-<.>) :: FilePath -> FilePath -> FilePath
(-<.>) = coerce (OsPath.-<.>)

(<.>) :: FilePath -> FilePath -> FilePath
(<.>) = coerce (OsPath.<.>)

(</>) :: FilePath -> FilePath -> FilePath
(</>) = coerce (OsPath.</>)

addExtension :: FilePath -> FilePath -> FilePath
addExtension = coerce OsPath.addExtension

addTrailingPathSeparator :: FilePath -> FilePath
addTrailingPathSeparator = coerce OsPath.addTrailingPathSeparator

combine :: FilePath -> FilePath -> FilePath
combine = coerce OsPath.combine

dropDrive :: FilePath -> FilePath
dropDrive = coerce OsPath.dropDrive

dropExtension :: FilePath -> FilePath
dropExtension = coerce OsPath.dropExtension

dropExtensions :: FilePath -> FilePath
dropExtensions = coerce OsPath.dropExtensions

dropFileName :: FilePath -> FilePath
dropFileName = coerce OsPath.dropFileName

dropTrailingPathSeparator :: FilePath -> FilePath
dropTrailingPathSeparator = coerce OsPath.dropTrailingPathSeparator

equalFilePath :: FilePath -> FilePath -> Bool
equalFilePath = coerce OsPath.equalFilePath

extSeparator :: Char
extSeparator = FilePath.extSeparator

getSearchPath :: IO [FilePath]
getSearchPath = fmap splitSearchPath (getEnv "PATH")
  where
    lookupEnv :: FilePath -> IO (Maybe FilePath)
    lookupEnv = coerce Posix.getEnv

    getEnv :: FilePath -> IO FilePath
    getEnv name = lookupEnv name >>= \ case
      Nothing -> ioe_missingEnvVar name.toString.unpack
      Just value -> return value

    ioe_missingEnvVar :: [Char] -> IO a
    ioe_missingEnvVar name = ioException (IOError Nothing NoSuchThing "getEnv" "no environment variable" Nothing (Just name))

hasDrive :: FilePath -> Bool
hasDrive = coerce OsPath.hasDrive

hasExtension :: FilePath -> Bool
hasExtension = coerce OsPath.hasExtension

hasTrailingPathSeparator :: FilePath -> Bool
hasTrailingPathSeparator = coerce OsPath.hasTrailingPathSeparator

isAbsolute :: FilePath -> Bool
isAbsolute = coerce OsPath.isAbsolute

isDrive :: FilePath -> Bool
isDrive = coerce OsPath.isDrive

isExtSeparator :: Char -> Bool
isExtSeparator = FilePath.isExtSeparator

isExtensionOf :: FilePath -> FilePath -> Bool
isExtensionOf = coerce OsPath.isExtensionOf

isPathSeparator :: Char -> Bool
isPathSeparator = FilePath.isPathSeparator

isRelative :: FilePath -> Bool
isRelative = coerce OsPath.isRelative

isSearchPathSeparator :: Char -> Bool
isSearchPathSeparator = FilePath.isSearchPathSeparator

isValid :: FilePath -> Bool
isValid = coerce OsPath.isValid

joinDrive :: FilePath -> FilePath -> FilePath
joinDrive = coerce OsPath.joinDrive

joinPath :: [FilePath] -> FilePath
joinPath = coerce OsPath.joinPath

makeRelative :: FilePath -> FilePath -> FilePath
makeRelative = coerce OsPath.makeRelative

makeValid :: FilePath -> FilePath
makeValid = coerce OsPath.makeValid

normalise :: FilePath -> FilePath
normalise = coerce OsPath.normalise

pathSeparator :: Char
pathSeparator = FilePath.pathSeparator

pathSeparators :: [Char]
pathSeparators = FilePath.pathSeparators

replaceBaseName :: FilePath -> FilePath -> FilePath
replaceBaseName = coerce OsPath.replaceBaseName

replaceDirectory :: FilePath -> FilePath -> FilePath
replaceDirectory = coerce OsPath.replaceDirectory

replaceExtension :: FilePath -> FilePath -> FilePath
replaceExtension = coerce OsPath.replaceExtension

replaceExtensions :: FilePath -> FilePath -> FilePath
replaceExtensions = coerce OsPath.replaceExtensions

replaceFileName :: FilePath -> FilePath -> FilePath
replaceFileName = coerce OsPath.replaceFileName

searchPathSeparator :: Char
searchPathSeparator = FilePath.searchPathSeparator

splitDirectories :: FilePath -> [FilePath]
splitDirectories = coerce OsPath.splitDirectories

splitDrive :: FilePath -> (FilePath, FilePath)
splitDrive = coerce OsPath.splitDrive

splitExtension :: FilePath -> (FilePath, FilePath)
splitExtension = coerce OsPath.splitExtension

splitExtensions :: FilePath -> (FilePath, FilePath)
splitExtensions = coerce OsPath.splitExtensions

splitFileName :: FilePath -> (FilePath, FilePath)
splitFileName = coerce OsPath.splitFileName

splitPath :: FilePath -> [FilePath]
splitPath = coerce OsPath.splitPath

splitSearchPath :: FilePath -> [FilePath]
splitSearchPath = coerce OsPath.splitSearchPath

stripExtension :: FilePath -> FilePath -> Maybe FilePath
stripExtension = coerce OsPath.stripExtension

takeBaseName :: FilePath -> FilePath
takeBaseName = coerce OsPath.takeBaseName

takeDirectory :: FilePath -> FilePath
takeDirectory = coerce OsPath.takeDirectory

takeDrive :: FilePath -> FilePath
takeDrive = coerce OsPath.takeDrive

takeExtension :: FilePath -> FilePath
takeExtension = coerce OsPath.takeExtension

takeExtensions :: FilePath -> FilePath
takeExtensions = coerce OsPath.takeExtensions

takeFileName :: FilePath -> FilePath
takeFileName = coerce OsPath.takeFileName
