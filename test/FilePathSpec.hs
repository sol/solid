{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module FilePathSpec (spec) where

import Helper hiding (shouldThrow)
import Test.Hspec (shouldThrow)
import GHC.IO.Exception
import System.IO.Error
import Gen qualified
import Range qualified

import qualified System.Directory.Import as Haskell

withPath :: (FilePath -> IO a) -> IO a
withPath action = inTempDirectory $ do
  action "foo.txt"

withFile :: (FilePath -> IO a) -> IO a
withFile action = inTempDirectory $ do
  let file = "foo.txt"
  touch file
  action file

withDirectory :: (FilePath -> IO a) -> IO a
withDirectory action = inTempDirectory $ do
  let dir = "foo"
  Haskell.createDirectory dir
  action dir

spec :: Spec
spec = do
  describe "Ord FilePath" $ do
    it "behaves like Ord String" $ do
      xs :: String <- forAll $ pack <$> Gen.list (Range.linear 1 10) Gen.unicodeAny
      ys :: String <- forAll $ pack <$> Gen.list (Range.linear 1 10) Gen.unicodeAny
      compare xs.asFilePath ys.asFilePath === compare xs ys

  describe ".toString" $ do
    it "converts a FilePath to a String" $ do
      let path = "foo.txt" :: FilePath
      path.toString `shouldBe` "foo.txt"

  describe "exists?" $ do
    context "with a non-existing path" $ do
      it "returns False" $ do
        withPath $ \ path -> do
          path.exists? `shouldReturn` False

    context "with a file" $ do
      it "returns True" $ do
        withFile $ \ file -> do
          file.exists? `shouldReturn` True

    context "with a directory" $ do
      it "returns True" $ do
        withDirectory $ \ dir -> do
          dir.exists? `shouldReturn` True

  describe "file?" $ do
    context "with a non-existing path" $ do
      it "returns False" $ do
        withPath $ \ path -> do
          path.file? `shouldReturn` False

    context "with a file" $ do
      it "returns True" $ do
        withFile $ \ file -> do
          file.file? `shouldReturn` True

    context "with a directory" $ do
      it "returns False" $ do
        withDirectory $ \ dir -> do
          dir.file? `shouldReturn` False

  describe "directory?" $ do
    context "with a non-existing path" $ do
      it "returns False" $ do
        withPath $ \ path -> do
          path.directory? `shouldReturn` False

    context "with a file" $ do
      it "returns False" $ do
        withFile $ \ file -> do
          file.directory? `shouldReturn` False

    context "with a directory" $ do
      it "returns True" $ do
        withDirectory $ \ dir -> do
          dir.directory? `shouldReturn` True

  describe "absolute" $ do
    it "makes a path absolute" $ do
      dir <- Directory.getCurrent
      let
        path = "foo" :: FilePath
        expected = dir </> path
      path.absolute `shouldReturn` expected
      FilePath.absolute path `shouldReturn` expected

  describe "remove" $ do
    context "with a non-existing path" $ do
      it "throws an exception" $ do
        withPath $ \ path -> do
          path.remove `shouldThrow` isDoesNotExistError

    context "with a file" $ do
      it "removes the file" $ do
        withFile $ \ file -> do
          file.remove
          file.exists? `shouldReturn` False

    context "with a directory" $ do
      it "removes the directory" $ do
        withDirectory $ \ dir -> do
          dir.remove
          dir.exists? `shouldReturn` False

      context "when the directory is not empty" $ do
        it "throws an exception" $ do
          withDirectory $ \ dir -> do
            touch (dir </> "foo")
            FilePath.remove dir `shouldThrow` (ioe_type >>> (== UnsatisfiedConstraints))

  describe "remove!" $ do
    context "with a non-existing path" $ do
      it "does nothing" $ do
        withPath $ \ path -> do
          path.remove!

    context "with a directory" $ do
      context "when the directory is not empty" $ do
        it "removes the directory" $ do
          withDirectory $ \ dir -> do
            touch (dir </> "foo")
            dir.remove!
            dir.exists? `shouldReturn` False

  describe "rename" $ do
    let dst = "bar"
    context "with a non-existing path" $ do
      it "throws an exception" $ do
        withPath $ \ path -> do
          path.rename dst `shouldThrow` isDoesNotExistError

    context "with a file" $ do
      it "renames the file" $ do
        withFile $ \ src -> do
          src.rename dst
          src.exists? `shouldReturn` False
          dst.exists? `shouldReturn` True

    context "with a directory" $ do
      it "renames the directory" $ do
        withDirectory $ \ src -> do
          src.rename dst
          src.exists? `shouldReturn` False
          dst.exists? `shouldReturn` True
