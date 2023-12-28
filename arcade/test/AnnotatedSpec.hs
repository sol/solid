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
    it "" $ do
      Annotated.splitAt 0 input `shouldBe` ([], input)

      Annotated.splitAt 5 input `shouldBe` ([chunk "foo", chunk "ba"], [chunk "r", chunk "baz"])

      Annotated.splitAt 6 input `shouldBe` ([chunk "foo", chunk "bar"], [chunk "baz"])
      Annotated.splitAt 9 input `shouldBe` (input, [])

  describe "chunksOf" $ do
    it "" $ do
      Annotated.chunksOf 2 input `shouldBe` [[chunk "fo"], [chunk "o", chunk "b"], [chunk "ar"], [chunk "ba"], [chunk "z"]]
