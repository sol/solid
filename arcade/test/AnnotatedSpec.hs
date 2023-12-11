{-# LANGUAGE OverloadedLists #-}
module AnnotatedSpec (spec) where

import Test.Hspec

use Annotated

spec :: Spec
spec = do
  let
    chunk = (,) ()
    input = [chunk "foo", chunk "bar", chunk "baz"]

  describe "splitAt" $ do
    it "splits an annotated string at a specified position" $ do
      Annotated.splitAt 6 input `shouldBe` ([chunk "foo", chunk "bar"], [chunk "baz"])

    context "when the specified position is within a chunk" $ do
      it "splits an annotated string at a specified position" $ do
        Annotated.splitAt 5 input `shouldBe` ([chunk "foo", chunk "ba"], [chunk "r", chunk "baz"])

    context "when the specified position is 0" $ do
      it "returns a tuple of the empty list and the original input" $ do
        Annotated.splitAt 0 input `shouldBe` ([], input)

    context "when the specified position equals the input size" $ do
      it "returns a tuple of the original input and the empty list" $ do
        Annotated.splitAt 9 input `shouldBe` (input, [])

  describe "chunksOf" $ do
    it "splits an annotated string into chunks of a specified size" $ do
      Annotated.chunksOf 2 input `shouldBe` [[chunk "fo"], [chunk "o", chunk "b"], [chunk "ar"], [chunk "ba"], [chunk "z"]]
