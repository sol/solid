{-# LANGUAGE LambdaCase #-}
module Main where

import Solid
import Solid.Foreign.Haskell qualified as Haskell

import qualified System.Environment as Haskell

import qualified Distribution.Client.Main as Cabal

import           Solid.PP qualified as PP
import           Solid.Driver (Mode(..), solid)
import qualified Solid.Driver as Driver

getArgs :: IO [String]
getArgs = map pack <$> Haskell.getArgs

withArgs :: [String] -> IO a -> IO a
withArgs = Haskell.withArgs . map unpack

getExecutablePath :: IO FilePath
getExecutablePath = do
  (sequence Haskell.executablePath >>= maybe Haskell.getProgName return . join) >>= Haskell.fromFilePath

main :: IO ()
main = getArgs >>= \ case
  "cabal" : args -> withArgs args Cabal.main
  [src, cur, dst, command] | command == Driver.desugarCommand -> PP.main src.unpack cur.unpack dst.unpack
  "ghc-options" : args -> (solid GhcOptions -< getExecutablePath) args
  args -> (solid Run -< getExecutablePath) args
