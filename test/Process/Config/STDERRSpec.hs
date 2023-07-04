{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Process.Config.STDERRSpec (spec) where

import Helper

echo :: Process.Config () () ()
echo = Process.shell "echo foo 1>&2"

spec :: Spec
spec = do
  describe "inherit" $ do
    it "inherits stderr" $ do
      hCapture_ [stderr] echo.stderr.inherit.run `shouldReturn` "foo\n"

  describe "null" $ do
    it "sets stderr to the null device" $ do
      hCapture_ [stderr] echo.stderr.null.run `shouldReturn` ""

  describe "capture" $ do
    it "captures stderr" $ do
      echo.stderr.capture.with Process.stderr `shouldReturn` "foo\n"

  describe "toStdout" $ do
    it "redirects stderr to stdout" $ do
      echo.stderr.toStdout.stdout.capture.with Process.stdout `shouldReturn` "foo\n"

  describe "useFile" $ do
    it "redirects stderr to a file" $ do
      Temp.withDirectory $ \ dir -> do
        let file = dir </> "foo.txt"
        (echo.stderr.useFile file).run
        readFile file `shouldReturn` "foo\n"

  describe "createPipe" $ do
    it "creates a pipe for stderr" $ do
      echo.stderr.createPipe.with $ \ process -> do
        process.stderr.getContents `shouldReturn` "foo\n"

  describe "useHandle" $ do
    it "uses a handle for stderr" $ do
      Temp.withFile $ \ name handle -> do
        (echo.stderr.useHandle handle).run
        handle.open? `shouldReturn` True
        handle.close
        readFile name `shouldReturn` "foo\n"

  describe "useAndCloseHandle" $ do
    it "uses a handle for stderr and closes that handle in the parent" $ do
      Temp.withFile $ \ name handle -> do
        (echo.stderr.useAndCloseHandle handle).run
        handle.open? `shouldReturn` False
        readFile name `shouldReturn` "foo\n"
