{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE BlockArguments #-}
module Solid.Driver (
  Mode(..)
, solid
, desugarCommand
) where

import Solid
import Solid.PP (Extension, extensions)
import Test.DocTest (doctest)

import System.Directory.Import

repository :: String
repository = "git@github.com:sol/solid.git"

revision :: String
revision = "a956385aa8815827e0f621eb518cb02184234732"

ghc :: String
ghc = "9.6.1"

desugarCommand :: String
desugarCommand = internalCommand "desugar"

internalCommand :: String -> String
internalCommand name = "{name}-{marker}"
  where
    marker :: String
    marker = "f817da5ee1a2164ad58986293141e5bf"

data Mode = GhcOptions | Doctest | With FilePath | Run

solid :: Mode -> FilePath -> [String] -> IO ()
solid mode self args = do
  cache <- getCacheDirectory
  ghc_dir <- determine_ghc_dir (cache </> "ghc-{ghc}".asFilePath)
  Env.path.extend ghc_dir do
    packageEnv <- ensurePackageEnv self cache
    let options = ghcOptions self packageEnv args
    case mode of
      GhcOptions -> stdout.print options.unlines
      Doctest -> doctest (options.map unpack)
      With command -> (Process.command command options).with Process.status >>= throwIO
      Run -> (Process.command (ghc_dir </> "runghc") options).with Process.status >>= throwIO

getCacheDirectory :: IO FilePath
getCacheDirectory = do
  cache <- getXdgDirectory XdgCache "solid"
  Directory.ensure cache
  return cache

determine_ghc_dir :: FilePath -> IO FilePath
determine_ghc_dir cache = do
  unless -< cache.exists? $ do
    dir <- find_ghc
    atomicWriteFile cache dir.toString
  String.asFilePath <$> readFile cache

find_ghc :: IO FilePath
find_ghc = Env.path.resolve "stack" >>= \ case
  Nothing -> Process.exit "{}: could not find stack"
  Just stack -> Temp.withDirectory $ \ tmp -> do
    let resolver = tmp </> "stackage.yaml"
    writeFile resolver "resolver:\n  compiler: ghc-{ghc}"
    (Process.command stack ["--resolver={resolver}", "path", "--compiler-bin"]).read <&> (.strip.asFilePath)

atomicWriteFile :: FilePath -> String -> IO ()
atomicWriteFile dst str = do
  let tmp = dst <.> "tmp"
  writeFile tmp str
  tmp.rename dst

ensurePackageEnv :: FilePath -> FilePath -> IO FilePath
ensurePackageEnv self cache = do
  unless -< packageEnv.exists? $ do
    Temp.withDirectoryAt cache $ \ tmp -> do
      Directory.withCurrent tmp do
        git ["init", "-q"]
        git ["remote", "add", "origin", repository]
        git ["fetch", "--depth", "1", "origin", revision, "-q"]
        git ["checkout", "FETCH_HEAD", "-q"]
        cabal $ "-v0" : "install" : "--package-env={packageEnv}" : "--lib" : packages
  return packageEnv
  where
    git :: [String] -> IO ()
    git args = (Process.command "git" args).run

    cabal :: [String] -> IO ()
    cabal args = (Process.command self ("cabal" : args)).run

    packages :: [String]
    packages = ["lib:solid", "lib:haskell-base"]

    packageEnv :: FilePath
    packageEnv = cache </> "{revision}-{packages.sort.unlines.md5sum}.env".asFilePath

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
