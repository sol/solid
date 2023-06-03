{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module ByteStringSpec (spec) where

import Helper

invalidUtf8 :: ByteString
invalidUtf8 = "foo " <> ByteString.pack [0xC3, 0x28] <> " bar"

spec :: Spec
spec = do
  describe "length" $ do
    it "returns the length of a ByteString" $ do
      ByteString.length "foo" `shouldBe` (3 :: Int)

  describe "asString" $ do
    it "converts a ByteString to a String" $ do
      ByteString.asString "foo" `shouldBe` Just "foo"

    context "on invalid UTF-8" $ do
      it "returns Nothing" $ do
        invalidUtf8.asString `shouldBe` Nothing

  describe "asString!" $ do
    it "converts a ByteString to a String" $ do
      ByteString.asString! "foo" `shouldBe` "foo"

    context "on invalid UTF-8" $ do
      it "throws an exception" $ do
        evaluate invalidUtf8.asString! `shouldThrow` UnicodeDecodeError

  describe "decodeUtf8" $ do
    it "converts a ByteString to a String" $ do
      ByteString.decodeUtf8 "foo" `shouldBe` "foo"

    context "on invalid UTF-8" $ do
      it "inserts Unicode replacement characters" $ do
        invalidUtf8.decodeUtf8 `shouldBe` "foo \xFFFD( bar"

  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      let input = "  foo\n \r" :: ByteString
      input.strip `shouldBe` "foo"

  describe "startsWith" $ do
    it "checks if a string starts with an other string" $ do
      let input = "123" :: ByteString
      input.startsWith "1" `shouldBe` True
      ByteString.startsWith "1" input `shouldBe` True

  describe "endsWith" $ do
    it "checks if a string ends with an other string" $ do
      let input = "123" :: ByteString
      input.endsWith "3" `shouldBe` True
      ByteString.endsWith "3" input `shouldBe` True

  describe "contains" $ do
    it "checks if a string contains an other string" $ do
      let input = "123" :: ByteString
      input.contains "2" `shouldBe` True

  describe "stripPrefix" $ do
    it "strips prefix" $ do
      let input = "foobar" :: ByteString
      input.stripPrefix "foo" `shouldBe` Just "bar"

  describe "stripSuffix" $ do
    it "strips suffix" $ do
      let input = "foobar" :: ByteString
      input.stripSuffix "bar" `shouldBe` Just "foo"
