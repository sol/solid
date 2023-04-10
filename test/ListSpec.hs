{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module ListSpec (spec) where

import Helper

spec :: Spec
spec = do
  describe ".startsWith" $ do
    it "checks if a string starts with an other string" $ do
      let input = [1, 2, 3] :: [Int]
      input.startsWith [1] `shouldBe` True

  describe ".endsWith" $ do
    it "checks if a string ends with an other string" $ do
      let input = [1, 2, 3] :: [Int]
      input.endsWith [3] `shouldBe` True

  describe ".contains" $ do
    it "checks if a string contains an other string" $ do
      let input = [1, 2, 3] :: [Int]
      input.contains [2] `shouldBe` True

  describe "nub" $ do
    it "removes duplicates from a list" $ do
      [1, 2, 3, 23, 1, 42, 1].nub `shouldBe` [1, 2, 3, 23, 42 :: Int]

  describe "nub!!" $ do
    it "removes duplicates from a list" $ do
      [1, 2, 3, 23, 1, 42, 1].nub!! `shouldBe` [1, 2, 3, 23, 42 :: Int]
