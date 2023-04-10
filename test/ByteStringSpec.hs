{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module ByteStringSpec (spec) where

import Helper

import ByteString qualified

invalidUtf8 :: ByteString
invalidUtf8 = Bytes "foo \xc3\x28 bar"

spec :: Spec
spec = do
  describe ".length" $ do
    it "returns the length of a ByteString" $ do
      ByteString.length "foo" `shouldBe` (3 :: Int)

  describe ".asString" $ do
    it "converts a ByteString to a String" $ do
      ByteString.asString "foo" `shouldBe` Just "foo"

    context "on invalid UTF-8" $ do
      it "returns Nothing" $ do
        invalidUtf8.asString `shouldBe` Nothing

  describe ".asString!" $ do
    it "converts a ByteString to a String" $ do
      ByteString.asString! "foo" `shouldBe` "foo"

    context "on invalid UTF-8" $ do
      it "throws an exception" $ do
        evaluate invalidUtf8.asString! `shouldThrow` UnicodeDecodeError

  describe ".decodeUtf8" $ do
    it "converts a ByteString to a String" $ do
      ByteString.decodeUtf8 "foo" `shouldBe` "foo"

    context "on invalid UTF-8" $ do
      it "inserts Unicode replacement characters" $ do
        invalidUtf8.decodeUtf8 `shouldBe` "foo \xFFFD( bar"
