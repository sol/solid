module SolidSpec (spec) where

import Helper

spec :: Spec
spec = do
  describe "[].map" $ do
    it "maps over a list" $ do
      [1..3].map succ `shouldBe` [2..4 :: Int]
