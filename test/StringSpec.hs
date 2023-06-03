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
