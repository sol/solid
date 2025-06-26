{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Solid.PP.NewLexer.TypeSpec (spec) where

import Data.Text qualified as Text
import           Test.Hspec

import           Solid.PP.NewLexer.Type

spec :: Spec
spec = do
  describe "index" do
    let
      t = "foobar"

    it "" do
      let lexer = new t
      index 5 lexer `shouldBe` 'r'
      index 6 lexer `shouldBe` '\0'

    it "" do
      let lexer = new $ Text.drop 1 t
      index 4 lexer `shouldBe` 'r'
      index 5 lexer `shouldBe` '\0'
