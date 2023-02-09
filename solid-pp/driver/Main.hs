{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           System.IO
import           System.Environment
import           System.Exit (exitFailure)

import qualified Solid.PP as PP

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src] -> run src src "/dev/stdout"
    [src, cur, dst] -> run src cur dst
    _ -> die $ \ name -> "Usage: " <> name <> " SRC CUR DST"

run :: String -> String -> String -> IO ()
run src cur dst = PP.run src cur dst >>= \ case
  PP.Failure err -> die $ \ _ -> err
  PP.Success -> return ()

die :: (String -> String) -> IO a
die err = getProgName >>= hPutStrLn stderr . err >> exitFailure
