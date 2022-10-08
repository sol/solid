{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.FilePathSpec (spec) where

import           Prelude ()
import           Solid

import           Test.Hspec
import           Test.QuickCheck
import           Test.Mockery.Directory

path :: FilePath
path = "foo.txt"

spec :: Spec
spec = do
  describe "Ord FilePath" $ do
    it "behaves like Ord [Char]" $ do
      property $ \ xs ys -> do
        compare xs.toFilePath ys.toFilePath `shouldBe` compare xs (ys :: [Char])

  describe ".toFilePath @[Char]" $ do
    it "converts a list of Char to a FilePath" $ do
      ("foo.txt" :: [Char]).toFilePath `shouldBe` path

  describe ".toFilePath @String" $ do
    it "converts a String to a FilePath" $ do
      ("foo.txt" :: String).toFilePath `shouldBe` path

  describe ".toString" $ do
    it "converts a FilePath to a String" $ do
      path.toString `shouldBe` ("foo.txt" :: String)

  describe ".exists?" $ around_ inTempDirectory $ do
    context "when path does not exist" $ do
      it "returns False" $ do
        path.exists? `shouldReturn` False

    context "when path refers to an existing file" $ do
      it "returns True" $ do
        touch "foo.txt"
        path.exists? `shouldReturn` True

    context "when path refers to an existing directory" $ do
      it "returns True" $ do
        touch "foo.txt/bar"
        path.exists? `shouldReturn` True
