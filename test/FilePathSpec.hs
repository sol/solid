{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module FilePathSpec (spec) where

import Helper hiding (shouldThrow)
import Test.Hspec (shouldThrow)
import System.IO.Error
import Gen qualified
import Range qualified

import qualified System.Directory as Haskell

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
  Haskell.createDirectory "foo"
  action "foo"

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

  describe "remove" $ do
    context "with a non-existing path" $ do
      it "throws an exception" $ do
        withPath $ \ path -> do
          path.remove `shouldThrow` isDoesNotExistError

    context "with a file" $ do
      it "removes given file" $ do
        withFile $ \ file -> do
          file.remove
          file.exists? `shouldReturn` False

    context "with a directory" $ do
      it "removes given directory" $ do
        withDirectory $ \ dir -> do
          dir.remove
          dir.exists? `shouldReturn` False

  describe "rename" $ do
    let dst = "bar"
    context "with a non-existing path" $ do
      it "throws an exception" $ do
        withPath $ \ path -> do
          path.rename dst `shouldThrow` isDoesNotExistError

    context "with a file" $ do
      it "renames given file" $ do
        withFile $ \ src -> do
          src.rename dst
          src.exists? `shouldReturn` False
          dst.exists? `shouldReturn` True

    context "with a directory" $ do
      it "renames given directory" $ do
        withDirectory $ \ src -> do
          src.rename dst
          src.exists? `shouldReturn` False
          dst.exists? `shouldReturn` True
