{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module IntSpec (spec) where

import Helper

use Int

ten :: Int
ten = 10

spec :: Spec
spec = do
  describe "negate" $ do
    it "negate" $ do
      ten.negate `shouldBe` -10
      Int.negate ten `shouldBe` -10

  describe "plus" $ do
    it "adds" $ do
      ten.plus 1 `shouldBe` 11
      Int.plus 1 ten `shouldBe` 11

  describe "minus" $ do
    it "subtracts" $ do
      ten.minus 1 `shouldBe` 9
      Int.minus 1 ten `shouldBe` 9

  describe "times" $ do
    it "multiplies" $ do
      ten.times 2 `shouldBe` 20
      Int.times 2 ten `shouldBe` 20

  describe "div" $ do
    it "divides" $ do
      ten.div 2 `shouldBe` 5
      Int.div 2 ten `shouldBe` 5
