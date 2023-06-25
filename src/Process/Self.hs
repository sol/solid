module Process.Self where

import Solid

import System.Environment.Import qualified as Haskell
import System.Exit (exitFailure)

name :: IO String
name = Haskell.getProgName

args :: IO [String]
args = Haskell.getArgs

exit :: (String -> String) -> IO a
exit message = do
  name >>= stderr.print . message
  exitFailure
