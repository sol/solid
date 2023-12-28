{-# LANGUAGE OverloadedLists #-}
module SyntaxSpec (spec) where

import           Test.Hspec

import           Syntax

spec :: Spec
spec = do
  describe "annotate" $ do
    it "reverses a list" $ do
      annotate "import Foo" `shouldBe` [(Just Include, "import"), (Nothing, " Foo")]
      annotate "  import Foo  " `shouldBe` [(Nothing, "  "), (Just Include, "import"), (Nothing, " Foo  ")]
      annotate "\{-# LANGUAGE OverloadedStrings #-}" `shouldBe` [(Just SpecialComment, "\{-# LANGUAGE OverloadedStrings #-}")]
      annotate "\{- foo bar baz -}" `shouldBe` [(Just Comment, "\{- foo bar baz -}")]

      annotate "\"foo-\{}-bar\"" `shouldBe` [(Just String, "\"foo-"), (Nothing, "\{}"), (Just String, "-bar\"")]
