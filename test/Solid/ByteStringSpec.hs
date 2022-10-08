module Solid.ByteStringSpec (spec) where

import           Test.Hspec

import           Prelude()
import           Solid

spec :: Spec
spec = do
  let
    input :: String
    input = "foo"

  describe ".asByteString" $ do
    it "converts a String to a ByteString" $ do
      input.asByteString `shouldBe` (Bytes "foo" :: ByteString)

  describe ".length" $ do
    it "returns the length of a ByteString" $ do
      input.length `shouldBe` (3 :: Int)
