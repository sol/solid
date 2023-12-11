module ArcadeSpec (spec) where

import           Test.Hspec

import           Arcade

spec :: Spec
spec = do
  pass
  {-
  describe "renderBuffer" $ do
    it "" $ do
      let
        buffer = Buffer {
          lines = [
              "foofoofoo"
            , "barbarbar"
            , "bazbazbaz"
            ]
        , position = Position 1 1
        }
      renderBuffer 6 3 buffer `shouldBe` [
          "foofoo"
        , "foo"
        , "barbar"
        ]

    it "" $ do
      let
        buffer = Buffer {
          lines = [
              "foofoofoo"
            , "barbarbar"
            , "bazbazbaz"
            ]
        , position = Position 2 1
        }
      renderBuffer 6 3 buffer `shouldBe` [
          "barbar"
        , "bar"
        , "bazbaz"
        ]
        -}
