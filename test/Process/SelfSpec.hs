{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Process.SelfSpec (spec) where

import Helper
import System.Environment.Import (withProgName)

spec :: Spec
spec = do
  describe "exit" $ do
    it "exits with an error message" $ do
      withProgName "foo" $ do
        hCapture_ [stderr] $ do
          Process.exit "\{}: some error" `shouldThrow` Process.ExitFailure 1
        `shouldReturn` "foo: some error\n"
