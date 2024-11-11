{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module System.Process.Import where

import Prelude
import System.Exit (ExitCode)
use Haskell
use System.Process as Haskell

shell :: String -> Haskell.CreateProcess
shell = Haskell.shell . unpack

readCreateProcess :: Haskell.CreateProcess -> String -> IO String
readCreateProcess p = fmap pack . Haskell.readCreateProcess p . unpack

readProcessWithExitCode :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
readProcessWithExitCode n args input = do
  name <- Haskell.toFilePath n
  (e, out, err) <- Haskell.readProcessWithExitCode name args.map(.unpack) input.unpack
  return (e, pack out, pack err)
