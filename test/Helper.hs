module Helper (module Helper) where

import           Prelude ()
import           Solid as Helper

import           Test.Hspec as Helper
import           Test.QuickCheck as Helper

import           Test.Mockery.Directory as Helper (inTempDirectory)
import qualified Test.Mockery.Directory as Mockery

touch :: FilePath -> IO ()
touch = Mockery.touch . unFilePath
