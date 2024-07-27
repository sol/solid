{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE BlockArguments #-}
module Solid.Driver (
  Mode(..)
, solid
, desugarCommand
, doctest
) where

import Solid
import Solid.PP (LanguageFlag(..), language, extensions, showExtension)
import Test.DocTest.Internal.Run (doctestWithRepl)

import System.Directory.Import

use Solid.Util
use Solid.Ansi

repository :: String
repository = "https://github.com/sol/solid.git"

ghc :: String
ghc = "9.8.2"

revision :: String
revision = "1bc5e26be7788e9bb68048d8fe0964154bd6ff1c"

libraries :: [String]
libraries = [
    "lib:solid"
  , "lib:haskell-base"
  ]

executables :: [String]
executables = [
    "ghc-bin:exe:repl"
  , "solid-doctest:exe:solid-doctest"
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
  runtime <- ensureRuntime -< getXdgDirectory XdgState "solid" $ self
  Env.path.extend runtime.ghc_dir do
    let options = ghcOptions self runtime.package_env args
    case mode of
      GhcOptions -> stdout.print options.unlines
      Repl -> do
        repl <- get_repl runtime
        Process.command.uncurry(repl <&> (<> options)).with Process.status >>= throwIO
      Doctest -> Process.command(runtime.bindir </> "solid-doctest", options).with Process.status >>= throwIO
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

doctest :: FilePath -> [String] -> IO ()
doctest runtime_dir options = do
  repl <- readRuntime runtime_dir >>= get_repl
  doctestWithRepl repl.bimap((.toString.unpack), map unpack) options.map(unpack)

get_repl :: Runtime -> IO (FilePath, [String])
get_repl runtime = do
  libdir <- (.decodeUtf8.strip) <$> Process.command(runtime.ghc_dir </> "ghc", ["--print-libdir"]).read
  return (runtime.bindir </> "repl", ["-B{libdir}", "--interactive"])

data Runtime = Runtime {
  ghc_dir :: FilePath
, package_env :: FilePath
, bindir :: FilePath
}

data Paths = Paths {
  base_dir :: FilePath
, ghc_dir_cache :: FilePath
, package_env :: FilePath
, bindir :: FilePath
}

pathsFrom :: FilePath -> Paths
pathsFrom base_dir = Paths {
  base_dir
, ghc_dir_cache = base_dir </> "ghc"
, package_env = base_dir </> "package.env"
, bindir = base_dir </> "bin"
}

ensureRuntime :: FilePath -> FilePath -> IO Runtime
ensureRuntime state_dir self = do
  unless -< runtime_dir.exists? $ do
    createRuntime store runtime_dir self
  readRuntime runtime_dir
  where
    runtime_dir = state_dir </> "ghc-{ghc}-{fingerprint}".asFilePath
    store = state_dir </> "store"

readRuntime :: FilePath -> IO Runtime
readRuntime base_dir = do
  ghc_dir <- readBinaryFile paths.ghc_dir_cache <&> ByteString.asFilePath
  return Runtime {
    ghc_dir
  , package_env = paths.package_env
  , bindir = paths.bindir
  }
  where
    paths :: Paths
    paths = pathsFrom base_dir

createRuntime :: FilePath -> FilePath -> FilePath -> IO ()
createRuntime store runtime_dir self = withConsole $ \ console -> do
  Util.createAtomic runtime_dir $ \ tmp -> do
    let paths = pathsFrom tmp
    ghc_dir <- install_ghc console self
    writeBinaryFile paths.ghc_dir_cache ghc_dir.asByteString
    Env.path.extend ghc_dir do
      createPackageEnv console store self paths

install_ghc :: Console -> FilePath -> IO FilePath
install_ghc console self = do
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

createPackageEnv :: Console -> FilePath -> FilePath -> Paths -> IO ()
createPackageEnv console store self paths = do
  console.info "Creating package environment...\n"
  let intensity = Ansi.Faint
  bracket_ (console.info intensity.set) (console.info intensity.reset) do
    Temp.withDirectoryAt paths.base_dir $ \ tmp -> do
      Directory.withCurrent tmp do
        git ["init", "-q"]
        git ["remote", "add", "origin", repository]
        git ["fetch", "--depth", "1", "origin", revision, "-q"]
        git ["checkout", "FETCH_HEAD", "-q"]
        cabal $ "install" : "--package-env={paths.package_env}" : "--lib" : libraries
        cabal $ "install" : "--installdir={paths.bindir}" : "--install-method=copy" : executables
        remove_base_package paths.package_env
  where
    git :: [String] -> IO ()
    git args = (console.command "git" args).run

    verbosity :: [String] -> [String]
    verbosity = console.tty?.fold ("-v0" :) id

    cabal :: [String] -> IO ()
    cabal args = (console.command self ("cabal" : "--store-dir" : store.toString : verbosity args)).run

    remove_base_package :: FilePath -> IO ()
    remove_base_package env = do
      xs <- readBinaryFile env
      writeBinaryFile env xs.lines.discard(ByteString.startsWith "package-id base-4").unlines
