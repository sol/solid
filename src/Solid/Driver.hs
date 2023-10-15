{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE BlockArguments #-}
module Solid.Driver (
  Mode(..)
, solid
, desugarCommand
) where

import Solid
import Solid.PP (LanguageFlag(..), language, extensions, showExtension)
import Test.DocTest.Internal.Run (doctestWithRepl)

import System.Directory.Import
import Solid.Ansi qualified as Ansi

repository :: String
repository = "https://github.com/sol/solid.git"

ghc :: String
ghc = "9.6.2"

revision :: String
revision = "d3cb9cf8fd363e5cda96c0e3f6d74fc87ea15eab"

libraries :: [String]
libraries = [
    "lib:solid"
  , "lib:haskell-base"
  ]

executables :: [String]
executables = [
    "ghc-bin:exe:repl"
  ]

fingerprint :: Fingerprint
fingerprint = (ghc : revision : libraries.sort <> executables.sort).unlines.md5sum

tty :: FilePath
tty = "/dev/tty"

data Console = Console {
  info :: String -> IO ()
, handle :: Handle
, tty? :: Bool
, release :: IO ()
}

instance HasField "command" Console (FilePath -> [String] -> Process.Config () () ()) where
  getField console name args = Process.command(name, args).stdout.useHandle(console.handle).stderr.useHandle console.handle

withConsole :: (Console -> IO a) -> IO a
withConsole = with console
  where
    console :: IO Console
    console = IO.try (tty.open IO.WriteMode) >>= \ case
      Left _ -> return Console {
        info = \ _ -> pass
      , handle = stderr
      , tty? = False
      , release = pass
      }
      Right handle -> return Console {
        info = \ output -> handle.write output >> handle.flush
      , handle = handle
      , tty? = True
      , release = handle.release
      }

desugarCommand :: String
desugarCommand = internalCommand "desugar"

internalCommand :: String -> String
internalCommand name = "{name}-{marker}"
  where
    marker :: String
    marker = "f817da5ee1a2164ad58986293141e5bf"

data Mode = GhcOptions | Repl | Doctest | With FilePath | Run

solid :: Mode -> FilePath -> [String] -> IO ()
solid mode self args = do
  runtime <- ensureRuntime self
  Env.path.extend runtime.ghc_dir do
    let options = ghcOptions self runtime.package_env args
    case mode of
      GhcOptions -> stdout.print options.unlines
      Repl -> do
        repl <- get_repl runtime.ghc_dir runtime.bindir
        Process.command.uncurry(repl <&> (<> options)).with Process.status >>= throwIO
      Doctest -> do
        repl <- get_repl runtime.ghc_dir runtime.bindir
        doctestWithRepl repl.bimap((.toString.unpack), map unpack) options.map(unpack)
      With command -> Process.command(command, options).with Process.status >>= throwIO
      Run -> Process.command(runtime.ghc_dir </> "runghc", options).with Process.status >>= throwIO

ghcOptions :: FilePath -> FilePath -> [String] -> [String]
ghcOptions self packageEnv args = opts ++ args
  where
    opts = "-package-env={packageEnv}"
      : "-package=process"
      : desugar ++ exts
    desugar = ["-F", "-pgmF={self}", "-optF={desugarCommand}"]
    exts = "-X" <> pack (show language) : map showLanguageFlag extensions

    showLanguageFlag :: LanguageFlag -> String
    showLanguageFlag = \ case
      Enable extension  -> "-X"   <> pack (showExtension extension)
      Disable extension -> "-XNo" <> pack (showExtension extension)

get_repl :: FilePath -> FilePath -> IO (FilePath, [String])
get_repl ghc_dir bindir = do
  libdir <- (.decodeUtf8.strip) <$> Process.command(ghc_dir </> "ghc", ["--print-libdir"]).read
  return (bindir </> "repl", ["-B{libdir}", "--interactive"])

data Runtime = Runtime {
  ghc_dir :: FilePath
, package_env :: FilePath
, bindir :: FilePath
}

ensureRuntime :: FilePath -> IO Runtime
ensureRuntime self = withConsole $ \ console -> do
  base_dir <- getBaseDirectory
  ghc_dir <- determine_ghc_dir console self (base_dir </> "ghc")
  Env.path.extend ghc_dir do
    (package_env, bindir) <- ensurePackageEnv console self base_dir
    return Runtime {
      ghc_dir
    , package_env
    , bindir
    }

getBaseDirectory :: IO FilePath
getBaseDirectory = do
  dir <- getXdgDirectory XdgState "solid" <&> (</> "ghc-{ghc}-{fingerprint}".asFilePath)
  Directory.ensure dir
  return dir

determine_ghc_dir :: Console -> FilePath -> FilePath -> IO FilePath
determine_ghc_dir console self cache = do
  unless -< valid? $ do
    find_ghc console self >>= write_cache
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

find_ghc :: Console -> FilePath -> IO FilePath
find_ghc console self = do
  console.info "Installing GHC {ghc}... "
  Temp.withDirectory $ \ tmp -> do
    let resolver = tmp </> "stackage.yaml"
    writeFile resolver "resolver:\n  compiler: ghc-{ghc}"
    ghc_dir <- stack(["--resolver={resolver}", "path", "--compiler-bin"]).with Process.stdout
      <* console.info "{String.ansi("✔").green}\n"
    return ghc_dir.strip.asFilePath
  where
    stack :: [String] -> Process.Config () (IO ByteString) ()
    stack args = (console.command self ("stack" : "--verbosity" : "error" : args)).stdout.capture

atomicWriteFile :: FilePath -> ByteString -> IO ()
atomicWriteFile dst str = do
  let tmp = dst <.> "tmp"
  writeBinaryFile tmp str
  tmp.rename dst

ensurePackageEnv :: Console -> FilePath -> FilePath -> IO (FilePath, FilePath)
ensurePackageEnv console self base_dir = do
  unless -< packageEnv.exists? $ do
    console.info "Creating package environment...\n"
    let intensity = Ansi.Faint
    bracket_ (console.info intensity.set) (console.info intensity.reset) do
      Temp.withDirectoryAt base_dir $ \ tmp -> do
        Directory.withCurrent tmp do
          git ["init", "-q"]
          git ["remote", "add", "origin", repository]
          git ["fetch", "--depth", "1", "origin", revision, "-q"]
          git ["checkout", "FETCH_HEAD", "-q"]
          cabal $ "install" : "--package-env={packageEnv}" : "--lib" : libraries
          cabal $ "install" : "--installdir={bindir}" : executables
  return (packageEnv, bindir)
  where
    git :: [String] -> IO ()
    git args = (console.command "git" args).run

    verbosity :: [String] -> [String]
    verbosity = console.tty?.fold ("-v0" :) id

    cabal :: [String] -> IO ()
    cabal args = (console.command self ("cabal" : verbosity args)).run

    packageEnv :: FilePath
    packageEnv = base_dir </> "package.env"

    bindir :: FilePath
    bindir = base_dir </> "bin"
