{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Process.Config.STDOUTSpec (spec) where

import Helper

echo :: Process.Config () () ()
echo = Process.command "echo" ["foo"]

spec :: Spec
spec = do
  describe "inherit" $ do
    it "inherits stdout" $ do
      hCapture_ [stdout] echo.stdout.inherit.run `shouldReturn` "foo\n"

  describe "null" $ do
    it "sets stdout to the null device" $ do
      hCapture_ [stdout] echo.stdout.null.run `shouldReturn` ""

  describe "capture" $ do
    it "captures stdout" $ do
      echo.stdout.capture.with Process.stdout `shouldReturn` "foo\n"

  describe "useFile" $ do
    it "redirects stdout to a file" $ do
      Temp.withDirectory $ \ dir -> do
        let file = dir </> "foo.txt"
        (echo.stdout.useFile file).run
        readFile file `shouldReturn` "foo\n"

  describe "createPipe" $ do
    it "creates a pipe for stdout" $ do
      echo.stdout.createPipe.with $ \ process -> do
        process.stdout.getContents `shouldReturn` "foo\n"

  describe "useHandle" $ do
    it "uses a handle for stdout" $ do
      Temp.withFile $ \ name handle -> do
        (echo.stdout.useHandle handle).run
        handle.open? `shouldReturn` True
        handle.close
        readFile name `shouldReturn` "foo\n"

  describe "useAndCloseHandle" $ do
    it "uses a handle for stdout and closes that handle in the parent" $ do
      Temp.withFile $ \ name handle -> do
        (echo.stdout.useAndCloseHandle handle).run
        handle.open? `shouldReturn` False
        readFile name `shouldReturn` "foo\n"
