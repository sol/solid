{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Solid

import System.Environment.Import hiding (getExecutablePath)

import qualified Distribution.Client.Main as Cabal

import           Solid.PP qualified as PP
import           Solid.Driver (Mode(..), solid)
import qualified Solid.Driver as Driver

getExecutablePath :: IO FilePath
getExecutablePath = sequence executablePath >>= maybe (String.asFilePath <$> getProgName) return . join

main :: IO ()
main = getArgs >>= \ case
  "cabal" : args -> withArgs args Cabal.main
  [src, cur, dst, command] | command == Driver.desugarCommand -> PP.main src.unpack cur.unpack dst.unpack
  "ghc-options" : args -> (solid GhcOptions -< getExecutablePath) args
  args -> (solid Run -< getExecutablePath) args
