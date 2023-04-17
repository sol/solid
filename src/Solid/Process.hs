module Solid.Process where

import           Solid

import qualified System.Process as Haskell
import           System.Exit (ExitCode)

readProcess :: FilePath -> [String] -> String -> IO String
readProcess name args input = pack <$> Haskell.readProcess (unFilePath name) (map unpack args) input.unpack

callProcess :: FilePath -> [String] -> IO ()
callProcess name = Haskell.callProcess (unFilePath name) . map unpack

rawSystem :: FilePath -> [String] -> IO ExitCode
rawSystem name = Haskell.rawSystem (unFilePath name) . map unpack
