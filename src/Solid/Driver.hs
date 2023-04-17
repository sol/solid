{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE BlockArguments #-}
module Solid.Driver (
  desugarCommand
, solid
) where

import           Solid

import           System.Environment (getProgName)
import           System.Exit (exitFailure)
import           System.Directory hiding (withCurrentDirectory)
import qualified System.Directory as Haskell
import qualified System.IO.Temp as Haskell

import           Solid.PP (Extension, extensions)
import           Solid.Process
import qualified Env

repository :: String
repository = "git@github.com:sol/solid.git"

revision :: String
revision = "7f31f817e75267a206f81a24352413fbdcf6032f"

ghc :: String
ghc = "9.4.4"

resolver :: String
resolver = "nightly-2023-02-14"

desugarCommand :: String
desugarCommand = internalCommand "desugar"

internalCommand :: String -> String
internalCommand name = "{name}-{marker}"
  where
    marker :: String
    marker = "f817da5ee1a2164ad58986293141e5bf"

solid :: FilePath -> [String] -> IO ()
solid self args = do
  cache <- getCacheDirectory
  ghc_dir <- determine_ghc_dir (cache </> "ghc-{ghc}".toFilePath)
  Env.path.extend ghc_dir do
    packageEnv <- ensurePackageEnv self cache
    runghc self ghc_dir packageEnv args

getCacheDirectory :: IO FilePath
getCacheDirectory = do
  cache <- getXdgDirectory XdgCache "solid"
  createDirectoryIfMissing True cache
  return cache.toFilePath

determine_ghc_dir :: FilePath -> IO FilePath
determine_ghc_dir cache = do
  unless -< cache.exists? $ do
    dir <- find_ghc
    atomicWriteFile cache dir.toString
  (.toFilePath) <$> readFile cache

find_ghc :: IO FilePath
find_ghc = Env.path.resolve "stack" >>= \ case
  Just stack -> (.strip.toFilePath) <$> readProcess stack ["--resolver={resolver}", "path", "--compiler-bin"] ""
  Nothing -> exit "{}: could not find stack"

atomicWriteFile :: FilePath -> String -> IO ()
atomicWriteFile dst str = do
  let tmp = dst <.> "tmp"
  writeFile tmp str
  tmp.rename dst

ensurePackageEnv :: FilePath -> FilePath -> IO FilePath
ensurePackageEnv self cache = do
  unless -< packageEnv.exists? $ do
    withTempDirectory cache "solid" $ \ tmp -> do
      withCurrentDirectory tmp do
        git ["init", "-q"]
        git ["remote", "add", "origin", repository]
        git ["fetch", "--depth", "1", "origin", revision, "-q"]
        git ["checkout", "FETCH_HEAD", "-q"]
        cabal ["-v0", "install", "--lib", "--package-env={packageEnv}"]
  return packageEnv
  where
    git :: [String] -> IO ()
    git = callProcess "git"

    cabal :: [String] -> IO ()
    cabal args = callProcess self $ "cabal" : args

    packageEnv :: FilePath
    packageEnv = cache </> "{revision}.env".toFilePath

runghc :: FilePath -> FilePath -> FilePath -> [String] -> IO ()
runghc self dir packageEnv args = rawSystem bin (opts ++ args) >>= throwIO
  where
    bin = dir </> "runghc"
    opts = "-package-env={packageEnv}"
      : "-package=base (Prelude as BasePrelude, System.Exit, Control.Monad, System.Environment)"
      : "-package=process"
      : "-package=solid with (Solid as Prelude)"
      : desugar ++ exts
    desugar = ["-F", "-pgmF={self}", "-optF={desugarCommand}"]
    exts = map showExtension extensions

    showExtension :: Extension -> String
    showExtension extension = "-X" <> pack (show extension)

withTempDirectory :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTempDirectory dir template action = Haskell.withTempDirectory (unFilePath dir) template.unpack (action . (.toFilePath))

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir = Haskell.withCurrentDirectory (unFilePath dir)

exit :: (String -> String) -> IO a
exit message = do
  name <- pack <$> getProgName
  stderr.print $ message name
  exitFailure
