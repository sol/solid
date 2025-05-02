{-# LANGUAGE OverloadedStrings #-}
module Solid.PP.LocationIndexSpec (spec) where

import           Test.Hspec

import           Solid.PP.LocationIndex

spec :: Spec
spec = do
  describe "reverse" $ do
    it "reverses a list" $ do
      let
        input = "foo\nbar\nbaz"
        index = locationIndex input
      map (`offsetLine` index) [0..10] `shouldBe` [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3]
