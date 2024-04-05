{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Temp (
  directory

, withDirectory
, withDirectoryAt

, createDirectory
, createDirectoryAt

, withFile
, withFileAt

, writeFile
, writeFileAt

, openFile
, openFileAt
) where

import Solid hiding (writeFile)
import Solid.Bytes.Unsafe

import System.Environment.Import (getProgName)
import System.Directory.Import (getTemporaryDirectory)

import Data.Coerce (coerce)
use Solid.OsString
import System.Posix.Temp.PosixString qualified as Posix

directory :: IO FilePath
directory = getTemporaryDirectory >>= FilePath.absolute

withDirectory :: (FilePath -> IO a) -> IO a
withDirectory = withDirectoryAt -< Temp.directory

withDirectoryAt :: FilePath -> (FilePath -> IO a) -> IO a
withDirectoryAt dir = bracket (createDirectoryAt dir) FilePath.remove!

createDirectory :: IO FilePath
createDirectory = createDirectoryAt -< Temp.directory

createDirectoryAt :: FilePath -> IO FilePath
createDirectoryAt = template >=> mkdtemp

withFile :: (FilePath -> Handle -> IO a) -> IO a
withFile = withFileAt -< Temp.directory

withFileAt :: FilePath -> (FilePath -> Handle -> IO a) -> IO a
withFileAt dir action = bracket (openFileAt dir) cleanup action.uncurry
  where
    cleanup :: (FilePath, Handle) -> IO ()
    cleanup (name, h) = h.close `finally` name.remove!

writeFile :: String -> IO FilePath
writeFile = writeFileAt -< Temp.directory

writeFileAt :: FilePath -> String -> IO FilePath
writeFileAt dir content = bracket (openFileAt dir) (IO.close . snd) $ \ (name, h) -> do
  h.write content
  return name

openFile :: IO (FilePath, Handle)
openFile = openFileAt -< Temp.directory

openFileAt :: FilePath -> IO (FilePath, Handle)
openFileAt = template >=> mkstemp

template :: FilePath -> IO FilePath
template dir = do
  name <- getProgName
  return $ dir </> name.asFilePath <> "-"

mkstemp :: FilePath -> IO (FilePath, Handle)
mkstemp = coerce Posix.mkstemp

mkdtemp :: FilePath -> IO FilePath
mkdtemp = coerce Posix.mkdtemp
