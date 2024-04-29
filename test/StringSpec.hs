{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module StringSpec (spec) where

import HaskellPrelude qualified as Haskell

import Helper
import Gen qualified
import Range qualified

listOfUpTo :: MonadGen m => Int -> m a -> m [a]
listOfUpTo n = Gen.list (Range.linear 0 n)

arbitrary :: MonadGen m => m String
arbitrary = Gen.string (Range.linear 0 10) Gen.unicodeScalar

spec :: Spec
spec = do
  describe "empty" $ do
    it "is the empty String" $ do
      String.empty `shouldBe` ""

  describe "empty?" $ do
    context "with the empty string" $ do
      it "returns True" $ do
        String.empty.empty? `shouldBe` True
        String.empty? String.empty `shouldBe` True

    context "with a non-empty string" $ do
      it "returns False" $ do
        String.empty? "foo" `shouldBe` False

  describe "length" $ do
    it "returns the length of a String" $ do
      input <- forAll arbitrary
      input.length === input.unpack.length
      String.length input === input.unpack.length

  describe "pack" $ do
    it "creates a String from a list of Char" $ do
      let input = "foo" :: [Char]
      input.pack `shouldBe` ("foo" :: String)
      String.pack input `shouldBe` ("foo" :: String)

  describe "unpack" $ do
    it "is inverse to pack" $ do
      input :: [Char] <- forAll $ listOfUpTo 10 Gen.unicodeScalar
      input.pack.unpack === input
      String.unpack (String.pack input) === input

  describe "lines" $ do
    it "breaks a string into separate lines" $ do
      input <- forAll $ Gen.string (Range.linear 0 100) $ Gen.frequency [
          (1, pure '\n')
        , (1, pure '\r')
        , (5, Gen.unicodeScalar)
        ]
      input.lines === map pack (Haskell.lines input.unpack)
      String.lines input === map pack (Haskell.lines input.unpack)

  describe "unlines" $ do
    it "joins lines, appending a terminating newline after each" $ do
      input :: [String] <- forAll $ listOfUpTo 10 arbitrary
      input.unlines === pack (Haskell.unlines (map unpack input))
      String.unlines input === pack (Haskell.unlines (map unpack input))

  describe "split" $ do
    it "splits a string" $ do
      let input = "foo, bar, baz"
      input.split ", " `shouldBe` ["foo", "bar", "baz"]
      String.split ", " input `shouldBe` ["foo", "bar", "baz"]

    context "with an empty separator" $ do
      it "splits a string" $ do
        let input = "foo"
        input.split "" `shouldBe` ["f", "o", "o"]
        String.split "" input `shouldBe` ["f", "o", "o"]

  describe "ljust" $ do
    it "pads the end of a string with spaces" $ do
      let input = "foo" :: String
      input.ljust 5 `shouldBe` "foo  "
      String.ljust 5 input `shouldBe` "foo  "

  describe "rjust" $ do
    it "pads the start of a string with spaces" $ do
      let input = "foo" :: String
      input.rjust 5 `shouldBe` "  foo"
      String.rjust 5 input `shouldBe` "  foo"

  describe "times" $ do
    it "replicates a string a given number of times" $ do
      let input = "foo" :: String
      input.times 3 `shouldBe` "foofoofoo"
      String.times 3 input `shouldBe` "foofoofoo"

    context "with 0" $ do
      it "returns the empty string" $ do
        let input = "foo" :: String
        String.times 0 input `shouldBe` ""

    context "with a negative number" $ do
      it "returns the empty string" $ do
        let input = "foo" :: String
        String.times (-1) input `shouldBe` ""

  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      let input = "  foo\n \r" :: String
      input.strip `shouldBe` "foo"
      String.strip input `shouldBe` "foo"

  describe "startsWith" $ do
    it "checks if a string starts with an other string" $ do
      let input = "123" :: String
      input.startsWith "1" `shouldBe` True
      String.startsWith "1" input `shouldBe` True

  describe "endsWith" $ do
    it "checks if a string ends with an other string" $ do
      let input = "123" :: String
      input.endsWith "3" `shouldBe` True
      String.endsWith "3" input `shouldBe` True

  describe "contains" $ do
    it "checks if a string contains an other string" $ do
      let input = "123" :: String
      input.contains "2" `shouldBe` True
      String.contains "2" input `shouldBe` True

  describe "stripPrefix" $ do
    it "strips prefix" $ do
      let input = "foobar" :: String
      input.stripPrefix "foo" `shouldBe` Just "bar"
      String.stripPrefix "foo" input `shouldBe` Just "bar"

  describe "stripSuffix" $ do
    it "strips suffix" $ do
      let input = "foobar" :: String
      input.stripSuffix "bar" `shouldBe` Just "foo"
      String.stripSuffix "bar" input `shouldBe` Just "foo"

  describe "asFilePath" $ do
    it "converts a String to a FilePath" $ do
      let path = "foo.txt" :: String
      path.asFilePath `shouldBe` "foo.txt"
      String.asFilePath path `shouldBe` "foo.txt"

  describe "read" $ do
    it "parses a value" $ do
      let input = "23" :: String
      input.read `shouldBe` Just (23 :: Int)
      String.read @Int input `shouldBe` Just 23

    context "with invalid input" $ do
      it "returns Nothing" $ do
        let input = "foo" :: String
        String.read @Int input `shouldBe` Nothing

  describe "read!" $ do
    it "parses a value" $ do
      let input = "23" :: String
      input.read! `shouldBe` (23 :: Int)
      String.read! @Int input `shouldBe` 23

    context "with invalid input" $ do
      it "throws an exception" $ do
        let input = "foo" :: String
        evaluate (input.read! :: Int) `shouldThrow?` invalidValue ["String.read!"] "no parse"
        evaluate (String.read! @Int input) `shouldThrow?` invalidValue ["read!"] "no parse"

  describe "ansi" $ do
    it "styles a string with ANSI escape sequences" $ do
      ("foo" :: String).ansi.red.toString `shouldBe` "\ESC[31mfoo\ESC[39m"
      (String.ansi "foo").red.toString `shouldBe` "\ESC[31mfoo\ESC[39m"
