module Solid.ToStringSpec (spec) where

import           Prelude ()
import           Helper

spec :: Spec
spec = do
  describe "toString" $ do
    it "converts a String to a String" $ do
      toString ("foo" :: String) `shouldBe` "foo"

    it "converts a Char to a String" $ do
      toString 'c' `shouldBe` "c"

    it "converts an Int to a String" $ do
      toString (23 :: Int) `shouldBe` "23"
