{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.PP.LexerSpec (spec) where

import           Prelude ()
import           Solid.PP.IO

import           Test.Hspec

import           Solid.PP.Lexer

instance IsString StringBuffer where
  fromString = stringToStringBuffer

spec :: Spec
spec = do
  describe "getExtensions" $ do
    it "extracts LANGUAGE pragmas from a module" $ do
      getExtensions [] "main.hs" "{-# LANGUAGE LambdaCase #-}" `shouldBe` [LambdaCase]

  describe "tokenizeWithErrors" $ do
    let
      input = unlines [
          "foo :: Int -> Int"
        , "foo = \\ case"
        , "  23 -> 42"
        , "  _  -> 65"
        ]

    it "reports missing extensions" $ do
      show . fst <$> tokenizeWithErrors [] "main.hs" input `shouldReturn` "main.hs:2:9: error: Illegal \\case"

    it "takes provided extensions into account" $ do
      show . fst <$> tokenizeWithErrors [LambdaCase] "main.hs" input `shouldReturn` ""

    it "takes LANGUAGE pragmas into account" $ do
      show . fst <$> tokenizeWithErrors [] "main.hs" ("{-# LANGUAGE LambdaCase #-}\n" <> input) `shouldReturn` ""
