{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE BlockArguments #-}
module Solid.Driver (
  Mode(..)
, solid
, desugarCommand
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
revision = "cd27310c76420cefbbf8a9665bb4caab2f963474"

ghc :: String
ghc = "9.6.1"

desugarCommand :: String
desugarCommand = internalCommand "desugar"

internalCommand :: String -> String
internalCommand name = "{name}-{marker}"
  where
    marker :: String
    marker = "f817da5ee1a2164ad58986293141e5bf"

data Mode = GhcOptions | Run

solid :: Mode -> FilePath -> [String] -> IO ()
solid mode self args = do
  cache <- getCacheDirectory
  ghc_dir <- determine_ghc_dir (cache </> "ghc-{ghc}".toFilePath)
  Env.path.extend ghc_dir do
    packageEnv <- ensurePackageEnv self cache
    let options = ghcOptions self packageEnv args
    case mode of
      GhcOptions -> stdout.print options.unlines
      Run -> rawSystem (ghc_dir </> "runghc") options >>= throwIO

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
  Nothing -> exit "{}: could not find stack"
  Just stack -> withSystemTempDirectory "stackage" $ \ tmp -> do
    let resolver = tmp </> "stackage.yaml"
    writeFile resolver "resolver:\n  compiler: ghc-{ghc}"
    readProcess stack ["--resolver={resolver}", "path", "--compiler-bin"] "" <&> (.strip.toFilePath)

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
        cabal $ "-v0" : "install" : "--package-env={packageEnv}" : "--lib" : packages
  return packageEnv
  where
    git :: [String] -> IO ()
    git = callProcess "git"

    cabal :: [String] -> IO ()
    cabal args = callProcess self $ "cabal" : args

    packages :: [String]
    packages = ["lib:solid", "lib:haskell-base"]

    packageEnv :: FilePath
    packageEnv = cache </> "{revision}-{packages.sort.unlines.md5sum}.env".toFilePath

ghcOptions :: FilePath -> FilePath -> [String] -> [String]
ghcOptions self packageEnv args = opts ++ args
  where
    opts = "-package-env={packageEnv}"
      : "-package=process"
      : desugar ++ exts
    desugar = ["-F", "-pgmF={self}", "-optF={desugarCommand}"]
    exts = map showExtension extensions

    showExtension :: Extension -> String
    showExtension extension = "-X" <> pack (show extension)

withTempDirectory :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTempDirectory dir template action = Haskell.withTempDirectory (unFilePath dir) template.unpack (action . (.toFilePath))

withSystemTempDirectory :: String -> (FilePath -> IO a) -> IO a
withSystemTempDirectory template action = Haskell.withSystemTempDirectory template.unpack (action . (.toFilePath))

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir = Haskell.withCurrentDirectory (unFilePath dir)

exit :: (String -> String) -> IO a
exit message = do
  name <- pack <$> getProgName
  stderr.print $ message name
  exitFailure
