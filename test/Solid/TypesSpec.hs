module Solid.TypesSpec (spec) where

import Helper
import Gen qualified
import Range qualified

import Solid.Types

listOfUpTo :: MonadGen m => Int -> m a -> m [a]
listOfUpTo n = Gen.list (Range.linear 0 n)

spec :: Spec
spec = do
  let
    input :: String
    input = "foo"

  describe ".asByteString" $ do
    it "converts a String to a ByteString" $ do
      input.asByteString `shouldBe` (Bytes "foo" :: ByteString)

  describe "Ord String" $ do
    it "behaves like Ord [Char]" $ do
      xs <- forAll $ listOfUpTo 10 Gen.unicodeScalar
      ys <- forAll $ listOfUpTo 10 Gen.unicodeScalar
      compare (pack xs :: String) (pack ys :: String) === compare xs ys
