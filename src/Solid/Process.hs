{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.Process where

import Solid
import Solid.Foreign.Haskell qualified as Haskell

import System.Process qualified as Haskell
import System.Exit (ExitCode)

readProcess :: FilePath -> [String] -> String -> IO String
readProcess name args input = pack <$> Haskell.readProcess (Haskell.toFilePath! name) (map unpack args) input.unpack

callProcess :: FilePath -> [String] -> IO ()
callProcess name = Haskell.callProcess (Haskell.toFilePath! name) . map unpack

rawSystem :: FilePath -> [String] -> IO ExitCode
rawSystem name = Haskell.rawSystem (Haskell.toFilePath! name) . map unpack
