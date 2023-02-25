module Main (main) where

import           Solid.PP.IO (die)
import qualified Solid.PP as PP
import           System.Environment

main :: IO ()
main = getArgs >>= \ case
  [src] -> PP.main src src "/dev/stdout"
  [src, cur, dst] -> PP.main src cur dst
  _ -> die $ \ name -> "Usage: " <> name <> " SRC CUR DST"
