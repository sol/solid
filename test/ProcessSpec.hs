{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module ProcessSpec (spec) where

import Helper

spec :: Spec
spec = do
  describe "ExitStatusException" $ do
    describe "toString" $ do
      context "with RawCommand" $ do
        it "converts to String" $ do
          let err = Process.ExitStatusException {
            status = 1
          , command = Process.RawCommand "echo" ["foo bar baz", "23"]
          }
          toString err `shouldBe` "Command `echo 'foo bar baz' 23` returned non-zero exit status 1."

      context "with ShellCommand" $ do
        it "converts to String" $ do
          let err = Process.ExitStatusException {
            status = 1
          , command = Process.ShellCommand "exit 1"
          }
          toString err `shouldBe` "Command `exit 1` returned non-zero exit status 1."

  describe "wait" $ do
    it "waits for process completion" $ do
      Temp.withDirectory $ \ dir -> do
        let
          file :: FilePath
          file = dir </> "foo"

          command :: Process.Config () () ()
          command = Process.raw "touch" [file.asByteString]

        command.start >>= Process.wait
        file.exists? `shouldReturn` True

    context "on non-zero exit status" $ do
      let
        command = Process.shell "exit 23"
        exitStatusException = Process.ExitStatusException {
          status = 23
        , command = Process.ShellCommand "exit 23"
        }

      it "throws ExitStatusException" $ do
        process <- command.start
        process.wait `shouldThrow` exitStatusException

      context "when called multiple times" $ do
        it "is idempotent" $ do
          process <- command.start
          process.wait `shouldThrow` exitStatusException
          process.wait `shouldThrow` exitStatusException

      context "when status is called before wait" $ do
        it "does not throw any exception" $ do
          process <- command.start
          process.status `shouldReturn` Process.ExitFailure 23
          process.wait

  describe "status" $ do
    it "returns the process exit status" $ do
      (Process.start "exit 23" >>= Process.status) `shouldReturn` Process.ExitFailure 23

  describe "checkStatus" $ do
    it "waits for process completion" $ do
      Process.start "exit 0" >>= Process.checkStatus

    context "on non-zero exit status" $ do
      it "throws ExitStatusException" $ do
        (Process.start "exit 1" >>= Process.checkStatus) `shouldThrow` Process.ExitStatusException {
          status = 1
        , command = Process.ShellCommand "exit 1"
        }

  describe "run" $ do
    it "waits for process completion" $ do
      Temp.withDirectory $ \ dir -> do
        let
          file :: FilePath
          file = dir </> "foo"

          command :: Process.Config () () ()
          command = Process.raw "touch" [file.asByteString]

        command.run
        file.exists? `shouldReturn` True

    context "on non-zero exit status" $ do
      it "throws ExitStatusException" $ do
        Process.run "exit 1" `shouldThrow` Process.ExitStatusException {
          status = 1
        , command = Process.ShellCommand "exit 1"
        }

  describe "read" $ do
    it "captures stdout" $ do
      Process.read "echo foo" `shouldReturn` "foo\n"

    context "on non-zero exit status" $ do
      it "throws ExitStatusException" $ do
        Process.read "echo foo && exit 1" `shouldThrow` Process.ExitStatusException {
          status = 1
        , command = Process.ShellCommand "echo foo && exit 1"
        }
