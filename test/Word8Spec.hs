{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Word8Spec (spec) where

import Helper

spec :: Spec
spec = do
  describe "read" $ do
    it "parses a number" $ do
      Word8.read "23" `shouldBe` Just 23

    context "with invalid input" $ do
      it "returns Nothing" $ do
        Word8.read "foo" `shouldBe` Nothing

  describe "read!" $ do
    it "parses a number" $ do
      Word8.read! "23" `shouldBe` 23

    context "with invalid input" $ do
      it "throws an exception" $ do
        evaluate (Word8.read! "foo") `shouldThrow?` invalidValue ["read!"] "no parse"
