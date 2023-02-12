{-# LANGUAGE LambdaCase #-}
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

import           Solid.PP (extensions)
import           Solid.PP.Lexer hiding (tokenize)
import qualified Solid.PP.Lexer as Lexer

instance IsString StringBuffer where
  fromString = stringToStringBuffer

instance IsString SourceText where
  fromString = SourceText

instance (Enum a, Show a) => Show (EnumSet a) where
  showsPrec n = showsPrec n . toList

instance (Enum a, Eq a) => Eq (EnumSet a) where
  a == b = toList a == toList b

instance Enum a => IsList (EnumSet a) where
  type Item (EnumSet a) = a
  fromList = EnumSet.fromList
  toList = EnumSet.toList

deriving instance Eq Token

tokenize :: String -> IO [Token]
tokenize = fmap (map unLoc) . Lexer.tokenize extensions "" . fromString

instance IsString Token where
  fromString = \ case
    ":" -> ITcolon
    "," -> ITcomma
    "[" -> ITobrack
    "]" -> ITcbrack
    "{" -> ITocurly
    "}" -> ITccurly
    "=" -> ITequal
    "+" -> ITvarsym "+"
    var -> ITvarid (fromString var)

instance Num Token where
  (+) = undefined
  (-) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger n = ITinteger IL {
    il_text = fromString (show n)
  , il_neg = False
  , il_value = n
  }

spec :: Spec
spec = do
  describe "tokenize" $ do
    it "accepts question marks at the end of identifiers" $ do
      tokenize "foo?" `shouldReturn` ["foo?"]

    it "accepts bangs at the end of identifiers" $ do
      tokenize "foo!" `shouldReturn` ["foo!"]

    context "when parsing string literals" $ do
      let
        string :: String -> Token
        string str = ITstring (SourceText $ show str) (fromString str)

        end_begin :: String -> Token
        end_begin str = ITstring_interpolation_end_begin (SourceText $ "}" <> str <> "{") (fromString str)

        end :: String -> Token
        end str = ITstring_interpolation_end (SourceText $ "}" <> str <> "\"") (fromString str)

        begin :: String -> Token
        begin str = ITstring_interpolation_begin (SourceText $ "\"" <> str <> "{") (fromString str)

      it "accepts string literals" $ do
        tokenize "\"foo\"" `shouldReturn` [string "foo"]

      it "attaches source ranges" $ do
        let range = ((.start) &&& (.end)) . getLoc
        [foo, foobar, _23] <- Lexer.tokenize extensions "" "  \"foo\"  \"foobar\" 23"
        range foo `shouldBe` (2, 7)
        range foobar `shouldBe` (9, 17)
        range _23 `shouldBe` (18, 20)

      it "accepts \\{ as {" $ do
        let src = "\"foo \\{ bar\""
        tokenize src `shouldReturn` [ITstring (fromString src) "foo { bar"]

      it "accepts string interpolation" $ do
        tokenize "\"foo { 23 } bar { 42 } baz\"" `shouldReturn` [
            begin "foo "
          , 23
          , end_begin " bar "
          , 42
          , end " baz"
          ]

      context "within interpolation" $ do
        it "accepts identifiers" $ do
          tokenize "\"foo { bar } baz\"" `shouldReturn` [
              begin "foo "
            , "bar"
            , end " baz"
            ]

        it "accepts integer literals" $ do
          tokenize "\"foo { bar 23 } baz\"" `shouldReturn` [
              begin "foo "
            , "bar"
            , 23
            , end " baz"
            ]

        it "accepts arithmetic expressions" $ do
          tokenize "\"foo { 23 + 42 } bar\"" `shouldReturn` [
              begin "foo "
            , 23
            , "+"
            , 42
            , end " bar"
            ]

        it "accepts string literals" $ do
          tokenize "\"foo { bar \"some string\" } baz\"" `shouldReturn` [
              begin "foo "
            , "bar"
            , string "some string"
            , end " baz"
            ]

        it "accepts interpolation in string literals" $ do
          tokenize "\"foo { bar \"some { 23 } string\" } baz\"" `shouldReturn` [
              begin "foo "
            , "bar", begin "some ", 23, end " string"
            , end " baz"
            ]

        it "accepts list literals" $ do
          tokenize "\"foo { [1, 2, 3] } baz\"" `shouldReturn` [
              begin "foo "
            , "[", 1, ",", 2, ",", 3, "]"
            , end " baz"
            ]

        it "accepts list construction" $ do
          tokenize "\"foo { x : xs } baz\"" `shouldReturn` [
              begin "foo "
            , "x", ":", "xs"
            , end " baz"
            ]

        context "within record updates" $ do
          it "accepts identifiers" $ do
            tokenize "\"foo { bar { someField = value } } baz\"" `shouldReturn` [
                begin "foo "
              , "bar", "{", "someField", "=", "value", "}"
              , end " baz"
              ]

          it "accepts integer literals" $ do
            tokenize "\"foo { bar { someField = 23 } } baz\"" `shouldReturn` [
                begin "foo "
              , "bar", "{", "someField", "=", 23, "}"
              , end " baz"
              ]

          it "accepts arithmetic expressions" $ do
            tokenize "\"foo { bar { someField = 23 + 42 } } baz\"" `shouldReturn` [
                begin "foo "
              , "bar", "{", "someField", "=", 23, "+", 42, "}"
              , end " baz"
              ]

          it "accepts string literals" $ do
            tokenize "\"foo { bar { someField = \"some string\" } } baz\"" `shouldReturn` [
                begin "foo "
              , "bar", "{", "someField", "=", string "some string", "}"
              , end " baz"
              ]

          it "accepts interpolation in string literals" $ do
            tokenize "\"foo { bar { someField = \"some { 23 } string\" } } baz\"" `shouldReturn` [
                begin "foo "
              , "bar", "{", "someField", "=", begin "some ", 23, end " string", "}"
              , end " baz"
              ]

          it "accepts list literals" $ do
            tokenize "\"foo { bar { someField = [1, 2, 3] } } baz\"" `shouldReturn` [
                begin "foo "
              , "bar", "{", "someField", "=", "[", 1, ",", 2, ",", 3, "]", "}"
              , end " baz"
              ]

          it "accepts list construction" $ do
            tokenize "\"foo { bar { someField = x : xs} } baz\"" `shouldReturn` [
                begin "foo "
              , "bar", "{", "someField", "=", "x", ":", "xs", "}"
              , end " baz"
              ]

          it "accepts nested record updates" $ do
            tokenize "\"{ foo { someField = bar { someOtherField = 23 } } }\"" `shouldReturn` [
                begin ""
              , "foo", "{", "someField", "=", "bar", "{", "someOtherField", "=", 23, "}", "}"
              , end ""
              ]

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
