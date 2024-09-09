{-# LANGUAGE OverloadedLists #-}
module SyntaxSpec (spec) where

import Test.Hspec

import Syntax

import Arcade ()

spec :: Spec
spec = do
  describe "annotate" $ do
    it "annotates imports" $ do
      annotate "  import Foo  " `shouldBe` [(Nothing, "  "), (Just Include, "import"), (Nothing, " Foo  ")]

    it "annotates language pragmas" $ do
      annotate "\{-# LANGUAGE OverloadedStrings #-}" `shouldBe` [(Just SpecialComment, "\{-# LANGUAGE OverloadedStrings #-}")]

    it "annotates comments" $ do
      annotate "\{- foo bar baz -}" `shouldBe` [(Just Comment, "\{- foo bar baz -}")]

    it "annotates interpolated strings" $ do
      annotate "\"foo-\{}-bar\"" `shouldBe` [(Just String, "\"foo-"), (Nothing, "\{}"), (Just String, "-bar\"")]

    it "annotates character escapes in string literals" $ do
      annotate (pack $ show @[Char] "foo\nbar\nbaz") `shouldBe` [
          (Just String, "\"foo")
        , (Just SpecialChar, "\\n")
        , (Just String, "bar")
        , (Just SpecialChar, "\\n")
        , (Just String, "baz\"")
        ]

  describe "breakOnCharacterEscape" $ do
    it "breaks a string on a character escape" $ do
      breakOnCharacterEscape (pack $ show @[Char] "foo \ESC bar") `shouldBe` Just ("\"foo ", "\\ESC", " bar\"")
