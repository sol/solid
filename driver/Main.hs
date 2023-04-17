{-# LANGUAGE LambdaCase #-}
module Main where

import           Solid

import qualified System.Environment as Haskell

import qualified Distribution.Client.Main as Cabal

import           Solid.PP qualified as PP
import           Solid.Driver (solid)
import qualified Solid.Driver as Driver

getArgs :: IO [String]
getArgs = map pack <$> Haskell.getArgs

withArgs :: [String] -> IO a -> IO a
withArgs = Haskell.withArgs . map unpack

getExecutablePath :: IO FilePath
getExecutablePath = (.toFilePath) <$> do
  sequence Haskell.executablePath >>= maybe Haskell.getProgName return . join

main :: IO ()
main = getArgs >>= \ case
  "cabal" : args -> withArgs args Cabal.main
  [src, cur, dst, command] | command == Driver.desugarCommand -> PP.main src.unpack cur.unpack dst.unpack
  args -> (solid -< getExecutablePath) args
