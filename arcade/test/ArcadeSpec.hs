module ArcadeSpec (spec) where

import           Test.Hspec

import           Arcade

spec :: Spec
spec = do
  describe "scrollDown" $ do
    it "" $ do
      let
        view = View {
            buffer = ""
          , width = 5
          , height = 10
          , offset = 1
          , line = 1
          , column = 1
          }
      view.scrollDown(1).offset `shouldBe` 1

    it "" $ do
      let
        view = View {
            buffer = "\n\n\n"
          , width = 5
          , height = 10
          , offset = 1
          , line = 1
          , column = 1
          }
      view.scrollDown(1).offset `shouldBe` 2
      view.scrollDown(1).line `shouldBe` 2
