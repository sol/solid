{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Solid

import System.Environment.Import (executablePath, withProgName, withArgs)

import qualified Distribution.Client.Main as Cabal
import qualified Stack

import           Solid.PP qualified as PP
import           Solid.Driver (Mode(..), solid)
import qualified Solid.Driver as Driver

getExecutablePath :: IO FilePath
getExecutablePath = sequence executablePath >>= maybe (String.asFilePath <$> Process.name) return . join

main :: IO ()
main = Process.args >>= \ case
  "cabal" : args -> withProgName "solid cabal" $ Cabal.main args.map(unpack)
  "stack" : args -> withProgName "solid stack" $ withArgs args Stack.main
  [src, cur, dst, command] | command == Driver.desugarCommand -> PP.main src.unpack cur.unpack dst.unpack
  "ghc-options" : args -> (solid GhcOptions -< getExecutablePath) args
  "repl" : args -> (solid Repl -< getExecutablePath) args
  "doctest" : args -> (solid Doctest -< getExecutablePath) args
  "with" : name : args -> (solid (With name.asFilePath) -< getExecutablePath) args
  args -> (solid Run -< getExecutablePath) args
