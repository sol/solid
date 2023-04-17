module Solid.CommonSpec (spec) where

import Helper

add :: Int -> Int -> Int
add = (+)

spec :: Spec
spec = do
  describe ".uncurry" $ do
    it "converts a curried function to a function on pairs" $ do
      add.uncurry (23, 42) `shouldBe` 65

  describe ".curry" $ do
    it "is inverse to uncurry" $ do
      add.uncurry.curry 23 42 `shouldBe` 65

  describe ".flip" $ do
    it "flips the first two arguments of a function" $ do
      map.flip [23, 42] succ `shouldBe` [24, 43 :: Int]

  describe ".swap" $ do
    it "swaps the components of a pair" $ do
      ("foo", 23).swap `shouldBe` (23 :: Int, "foo" :: String)
