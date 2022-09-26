module Main (main) where

import           System.IO
import           System.Environment
import           System.Exit

import qualified Solid.PP as PP

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src, cur, dst] -> PP.run src cur dst
    _ -> do
      name <- getProgName
      hPutStrLn stderr ("Usage: " <> name <> " SRC CUR DST")
      exitFailure
