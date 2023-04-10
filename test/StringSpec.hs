module StringSpec (spec) where

import Prelude qualified

import Helper
import Gen qualified
import Range qualified

listOfUpTo :: MonadGen m => Int -> m a -> m [a]
listOfUpTo n = Gen.list (Range.linear 0 n)

spec :: Spec
spec = do
  describe ".length" $ do
    it "returns the length of a String" $ do
      xs <- forAll $ listOfUpTo 10 Gen.unicodeAny
      xs.pack.length === xs.length

  describe "pack" $ do
    it "creates a String from a list of Char" $ do
      pack ("foo" :: [Char]) `shouldBe` ("foo" :: String)

  describe "unpack" $ do
    it "is inverse to pack" $ do
      xs <- forAll $ listOfUpTo 10 Gen.unicodeScalar
      unpack (pack xs) === xs

  describe "lines" $ do
    it "breaks a string into separate lines" $ do
      xs :: [Char] <- forAll $ listOfUpTo 100 $ Gen.frequency [
          (1, pure '\n')
        , (1, pure '\r')
        , (5, Gen.unicodeScalar)
        ]
      map unpack xs.pack.lines === Prelude.lines xs

  describe "unlines" $ do
    it "joins lines, appending a terminating newline after each" $ do
      xs :: [String] <- forAll $ listOfUpTo 10 (pack <$> listOfUpTo 10 Gen.unicodeAny)
      xs.unlines === pack (Prelude.unlines (map unpack xs))

  describe ".startsWith" $ do
    it "checks if a string starts with an other string" $ do
      let input = "123" :: String
      input.startsWith "1" `shouldBe` True

  describe ".endsWith" $ do
    it "checks if a string ends with an other string" $ do
      let input = "123" :: String
      input.endsWith "3" `shouldBe` True

  describe ".contains" $ do
    it "checks if a string contains an other string" $ do
      let input = "123" :: String
      input.contains "2" `shouldBe` True
