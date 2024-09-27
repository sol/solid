module Main (main) where

use System.Environment.Import
use Solid.Driver

getExecutablePath :: IO FilePath
getExecutablePath = (join <$> sequence Import.executablePath) >>= maybe exit return
  where
    exit = Process.exit "\{}: could not query the path to the current executable"

getRuntimeDirectory :: IO FilePath
getRuntimeDirectory = getExecutablePath <&> (.parent.parent)

main :: IO ()
main = do
  Driver.doctest -< getRuntimeDirectory -< Process.args
