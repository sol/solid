{-# LANGUAGE OverloadedStrings #-}
module Solid.PP.LocationIndexSpec (spec) where

import Data.Text.Internal qualified as Internal
import Data.Text (Text)
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

    it "reverses a list" $ do
     let
       input = "foo\nbar\nbaz"
       index = locationIndex input
     map (`offsetColumn` index) [0..10] `shouldBe` [1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3]

    it "reverses a list" $ do
      let
        input :: Text
        input = "fλλ"
        -- "foo\n我超爱中国菜\nbaz"
        index = locationIndex input

        len = case input of
          Internal.Text _ _ l -> l

      map (`offsetColumn` index) [0..len] `shouldBe` [1, 2, 3, 3, 4, 4]
