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
revision = "10ad38f69c92203776fbbb81462cbdd8f1528e5d"

ghc :: String
ghc = "9.6.2"

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
  ghc_dir <- determine_ghc_dir self (cache </> "ghc-{ghc}".asFilePath)
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

determine_ghc_dir :: FilePath -> FilePath -> IO FilePath
determine_ghc_dir self cache = do
  unless -< valid? $ do
    find_ghc self >>= write_cache
  read_cache
  where
    read_cache :: IO FilePath
    read_cache = readBinaryFile cache <&> ByteString.asFilePath

    write_cache :: FilePath -> IO ()
    write_cache = FilePath.asByteString >>> atomicWriteFile cache

    valid? :: IO Bool
    valid? = cache.exists? >>= \ case
      False -> return False
      True -> read_cache >>= FilePath.exists?

find_ghc :: FilePath -> IO FilePath
find_ghc self = do
  Temp.withDirectory $ \ tmp -> do
    let resolver = tmp </> "stackage.yaml"
    writeFile resolver "resolver:\n  compiler: ghc-{ghc}"
    (stack ["--resolver={resolver}", "path", "--compiler-bin"]).read <&> (.strip.asFilePath)
  where
    stack :: [String] -> Process.Config () () ()
    stack args = Process.command self ("stack" : args)

atomicWriteFile :: FilePath -> ByteString -> IO ()
atomicWriteFile dst str = do
  let tmp = dst <.> "tmp"
  writeBinaryFile tmp str
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
