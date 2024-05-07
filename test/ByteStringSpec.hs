{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module ByteStringSpec (spec) where

import Helper
import Gen qualified
import Range qualified

invalidUtf8 :: ByteString
invalidUtf8 = "foo " <> ByteString.pack [0xC3, 0x28] <> " bar"

spec :: Spec
spec = do
  describe "asString" $ do
    it "converts a ByteString to a String" $ do
      let input = "foo" :: ByteString
      input.asString `shouldBe` Just "foo"
      ByteString.asString input `shouldBe` Just "foo"

    context "on invalid UTF-8" $ do
      it "returns Nothing" $ do
        invalidUtf8.asString `shouldBe` Nothing

  describe "asString!" $ do
    it "converts a ByteString to a String" $ do
      let input = "foo" :: ByteString
      input.asString! `shouldBe` "foo"
      ByteString.asString! input `shouldBe` "foo"

    context "on invalid UTF-8" $ do
      it "throws an exception" $ do
        evaluate invalidUtf8.asString! `shouldThrow` UnicodeDecodeError

  describe "decodeUtf8" $ do
    it "converts a ByteString to a String" $ do
      let input = "foo" :: ByteString
      input.decodeUtf8 `shouldBe` "foo"
      ByteString.decodeUtf8 input `shouldBe` "foo"

    context "on invalid UTF-8" $ do
      it "inserts Unicode replacement characters" $ do
        invalidUtf8.decodeUtf8 `shouldBe` "foo \xFFFD( bar"

  describe "length" $ do
    it "returns the length of a ByteString" $ do
      input <- forAll $ Gen.bytes (Range.linear 0 10)
      input.length === input.unpack.length
      ByteString.length input === input.unpack.length

  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      let input = "  foo\n \r" :: ByteString
      input.strip `shouldBe` "foo"
      ByteString.strip input `shouldBe` "foo"

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
      ByteString.contains "2" input `shouldBe` True

  describe "stripPrefix" $ do
    it "strips prefix" $ do
      let input = "foobar" :: ByteString
      input.stripPrefix "foo" `shouldBe` Just "bar"
      ByteString.stripPrefix "foo" input `shouldBe` Just "bar"

  describe "stripSuffix" $ do
    it "strips suffix" $ do
      let input = "foobar" :: ByteString
      input.stripSuffix "bar" `shouldBe` Just "foo"
      ByteString.stripSuffix "bar" input `shouldBe` Just "foo"

  describe "asFilePath" $ do
    it "converts a ByteString to a FilePath" $ do
      let path = "foo.txt" :: ByteString
      path.asFilePath `shouldBe` "foo.txt"
      ByteString.asFilePath path `shouldBe` "foo.txt"

  describe "read" $ do
    it "parses a value" $ do
      let input = "23" :: ByteString
      input.read `shouldBe` Just (23 :: Int)
      ByteString.read @Int input `shouldBe` Just 23

    context "with invalid input" $ do
      it "returns Nothing" $ do
        let input = "foo" :: ByteString
        ByteString.read @Int input `shouldBe` Nothing

  describe "read!" $ do
    it "parses a value" $ do
      let input = "23" :: ByteString
      input.read! `shouldBe` (23 :: Int)
      ByteString.read! @Int input `shouldBe` 23

    context "with invalid input" $ do
      it "throws an exception" $ do
        let input = "foo" :: ByteString
        evaluate (input.read! :: Int) `shouldThrow?` invalidValue ["ByteString.read!"] "no parse"
        evaluate (ByteString.read! @Int input) `shouldThrow?` invalidValue ["read!"] "no parse"
