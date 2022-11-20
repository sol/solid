{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Helper (module Helper) where

import           Prelude ()
import           Solid as Helper

import           Test.Hspec as Helper hiding (shouldThrow)
import           Test.QuickCheck as Helper

import           Test.Mockery.Directory as Helper (inTempDirectory)
import qualified Test.Mockery.Directory as Mockery

import           Data.Typeable (typeOf)

import qualified Platform

touch :: FilePath -> IO ()
touch = Mockery.touch . unFilePath

infix 1 `shouldThrow`

shouldThrow :: (HasCallStack, Eq e, Exception e) => IO a -> e -> Expectation
action `shouldThrow` e = do
  r <- try action
  case r of
    Right _ -> expectationFailure $ unpack "did not get expected exception: {typeOf e}"
    Left err -> err `shouldBe` e

isADirectoryError :: FilePath -> IOException
isADirectoryError = if Platform.windows? then PermissionError else IsADirectoryError
