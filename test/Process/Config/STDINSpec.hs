{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Process.Config.STDINSpec (spec) where

import Helper
import Solid.Foreign.Haskell qualified as Haskell

import System.Posix.Types
import System.Posix.IO.PosixString

withHandle :: String -> (Handle -> IO a) -> IO a
withHandle content action = Temp.withFile $ \ _ handle -> do
  handle.write content
  handle.rewind
  action handle

cat :: Process.Config () () ()
cat = Process.command "cat" []

duplicateTo :: FilePath -> Fd -> IO a -> IO a
duplicateTo file dst action = do
  backup <- dup dst
  fd <- openFd (Haskell.asPlatformPath file) ReadWrite defaultFileFlags
  bracket_ (dupTo fd dst) (dupTo backup dst) action

spec :: Spec
spec = do
  describe "inherit" $ do
    it "inherits stdin" $ do
      Temp.withDirectory $ \ dir -> do
        let file = dir </> "stdin.txt"
        writeFile file "foo"
        stdin.withLock $ do
          file `duplicateTo` stdInput $ do
            cat.stdin.inherit.read `shouldReturn` "foo"

  describe "null" $ do
    it "sets stdin to the null device" $ do
      cat.stdin.null.read `shouldReturn` ""

  describe "set" $ do
    it "provides a String as stdin" $ do
      (cat.stdin.set "foo").read `shouldReturn` "foo"

  describe "setBytes" $ do
    it "provides a sequence of bytes as stdin" $ do
      (cat.stdin.setBytes ("foo" :: String)).read `shouldReturn` "foo"

  describe "setByteString" $ do
    it "provides a ByteString as stdin" $ do
      (cat.stdin.setByteString "foo").read `shouldReturn` "foo"

  describe "createPipe" $ do
    it "creates a pipe for stdin" $ do
      Process.with cat.stdout.capture.stdin.createPipe $ \ process -> do
        process.stdin.write "foo"
        process.stdin.close
        process.stdout `shouldReturn` "foo"

  describe "useHandle" $ do
    it "uses a handle for stdin" $ do
      withHandle "foo" $ \ handle -> do
        (cat.stdin.useHandle handle).read `shouldReturn` "foo"
        handle.open? `shouldReturn` True

  describe "useAndCloseHandle" $ do
    it "uses a handle for stdin and closes that handle in the parent" $ do
      withHandle "foo" $ \ handle -> do
        (cat.stdin.useAndCloseHandle handle).read `shouldReturn` "foo"
        handle.open? `shouldReturn` False
