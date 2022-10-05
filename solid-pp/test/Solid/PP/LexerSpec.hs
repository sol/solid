{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.PP.LexerSpec (spec) where

import           Prelude ()
import           Solid.PP.IO

import           Test.Hspec

import qualified Data.List as L
import           GHC.IsList
import           GHC.Data.EnumSet (EnumSet)
import qualified GHC.Data.EnumSet as EnumSet

import           Solid.PP.Lexer

instance IsString StringBuffer where
  fromString = stringToStringBuffer

instance (Enum a, Show a) => Show (EnumSet a) where
  showsPrec n = showsPrec n . toList

instance (Enum a, Eq a) => Eq (EnumSet a) where
  a == b = toList a == toList b

instance Enum a => IsList (EnumSet a) where
  type Item (EnumSet a) = a
  fromList = EnumSet.fromList
  toList = EnumSet.toList

spec :: Spec
spec = do
  describe "applyLanguagePragmas" $ do
    it "applies module LANGUAGE pragmas" $ do
      applyLanguagePragmas [] "main.hs" "{-# LANGUAGE LambdaCase #-}" `shouldBe` [LambdaCase]

    it "accepts negated LANGUAGE pragmas" $ do
      applyLanguagePragmas [LambdaCase] "main.hs" "{-# LANGUAGE NoLambdaCase #-}" `shouldBe` []

    context "with multiple LANGUAGE pragmas" $ do
      it "gives later appearances precedence" $ do
        let
          input = (fromString . L.unlines) [
              "{-# LANGUAGE NoLambdaCase #-}"
            , "{-# LANGUAGE LambdaCase #-}"
            , "{-# LANGUAGE NoLambdaCase #-}"
            , "{-# LANGUAGE NoOverloadedStrings #-}"
            , "{-# LANGUAGE OverloadedStrings #-}"
            ]
        applyLanguagePragmas [LambdaCase] "main.hs" input `shouldBe` [OverloadedStrings]

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

    it "takes negated LANGUAGE pragmas into account" $ do
      show . fst <$> tokenizeWithErrors [LambdaCase] "main.hs" ("{-# LANGUAGE NoLambdaCase #-}\n" <> input) `shouldReturn` "main.hs:3:9: error: Illegal \\case"
