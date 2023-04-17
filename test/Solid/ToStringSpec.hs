module Solid.ToStringSpec (spec) where

import           Helper

spec :: Spec
spec = do
  describe "toString" $ do
    it "converts String to String" $ do
      toString ("foo" :: String) `shouldBe` "foo"

    it "converts [Char] to String" $ do
      toString ("foo" :: [Char]) `shouldBe` "foo"

    it "converts Char to String" $ do
      toString 'c' `shouldBe` "c"

    it "converts Int to String" $ do
      toString (23 :: Int) `shouldBe` "23"
