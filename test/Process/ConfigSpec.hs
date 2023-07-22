{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Process.ConfigSpec (spec) where

import Helper

spec :: Spec
spec = do
  describe "setEnv" $ do
    it "sets the environment" $ do
      let
        config :: Process.Config () () ()
        config = (Process.command "env" []).setEnv [("FOO", "23")]
      config.read `shouldReturn` "FOO=23\n"

  describe "chdir" $ do
    it "sets the current working directory" $ do
      Temp.withDirectory $ \ dir -> do
        let
          config :: Process.Config () () ()
          config = (Process.command "pwd" []).chdir dir
        config.read `shouldReturn` dir.asByteString <> "\n"
