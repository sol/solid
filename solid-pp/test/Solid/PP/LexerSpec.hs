{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Solid.PP.LexerSpec (spec) where

import           Prelude ()
import           Solid.PP.IO

import           Test.Hspec

import           Solid.PP (language, extensions)
import           Solid.PP.Lexer.Extensions
import           Solid.PP.Lexer hiding (tokenize)
import qualified Solid.PP.Lexer as Lexer

instance IsString SourceText where
  fromString = SourceText . fromString

tokenize :: HasCallStack => String -> [Token]
tokenize = either error (map unLoc . (.tokens)) . Lexer.tokenize language extensions "" 1 . fromString

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
      tokenize "foo?" `shouldBe` ["foo?"]

    context "when parsing identifiers that end with a bang" $ do
      it "accepts bangs at the end of identifiers" $ do
        tokenize "foo!" `shouldBe` ["foo!"]

      it "accepts infix dot syntax" $ do
        tokenize "foo!.bar!" `shouldBe` ["foo!", InfixProjection, "bar!"]

      it "accepts prefix dot syntax" $ do
        tokenize "foo! .bar!" `shouldBe` ["foo!", PrefixProjection, "bar!"]

    context "when parsing string literals" $ do
      let
        string :: String -> Token
        string str = ITstring (fromString $ show str) (fromString str)

        end_begin :: String -> Token
        end_begin str = ITstring_interpolation_end_begin (fromString $ "}" <> str <> "\\{") (fromString str)

        end :: String -> Token
        end str = ITstring_interpolation_end (fromString $ "}" <> str <> "\"") (fromString str)

        begin :: String -> Token
        begin str = ITstring_interpolation_begin (fromString $ "\"" <> str <> "\\{") (fromString str)

      it "accepts string literals" $ do
        tokenize "\"foo\"" `shouldBe` [string "foo"]

      it "attaches source ranges" $ do
        let range = ((.start) &&& (.end)) . getLoc
        let Right [foo, foobar, _23] = (.tokens) <$> Lexer.tokenize language extensions "" 1 "  \"foo\"  \"foobar\" 23"
        range foo `shouldBe` (2, 7)
        range foobar `shouldBe` (9, 17)
        range _23 `shouldBe` (18, 20)

      it "accepts literal }" $ do
        let src = "\"foo } bar\""
        tokenize src `shouldBe` [ITstring (fromString src) "foo } bar"]

      it "accepts string interpolation" $ do
        tokenize "\"foo \\{ 23 } bar \\{ 42 } baz\"" `shouldBe` [
            begin "foo "
          , 23
          , end_begin " bar "
          , 42
          , end " baz"
          ]

      context "within interpolation" $ do
        it "accepts identifiers" $ do
          tokenize "\"foo \\{ bar } baz\"" `shouldBe` [
              begin "foo "
            , "bar"
            , end " baz"
            ]

        it "accepts integer literals" $ do
          tokenize "\"foo \\{ bar 23 } baz\"" `shouldBe` [
              begin "foo "
            , "bar"
            , 23
            , end " baz"
            ]

        it "accepts arithmetic expressions" $ do
          tokenize "\"foo \\{ 23 + 42 } bar\"" `shouldBe` [
              begin "foo "
            , 23
            , "+"
            , 42
            , end " bar"
            ]

        it "accepts string literals" $ do
          tokenize "\"foo \\{ bar \"some string\" } baz\"" `shouldBe` [
              begin "foo "
            , "bar"
            , string "some string"
            , end " baz"
            ]

        it "accepts interpolation in string literals" $ do
          tokenize "\"foo \\{ bar \"some \\{ 23 } string\" } baz\"" `shouldBe` [
              begin "foo "
            , "bar", begin "some ", 23, end " string"
            , end " baz"
            ]

        it "accepts list literals" $ do
          tokenize "\"foo \\{ [1, 2, 3] } baz\"" `shouldBe` [
              begin "foo "
            , "[", 1, ",", 2, ",", 3, "]"
            , end " baz"
            ]

        it "accepts list construction" $ do
          tokenize "\"foo \\{ x : xs } baz\"" `shouldBe` [
              begin "foo "
            , "x", ":", "xs"
            , end " baz"
            ]

        context "within record updates" $ do
          it "accepts identifiers" $ do
            tokenize "\"foo \\{ bar { someField = value } } baz\"" `shouldBe` [
                begin "foo "
              , "bar", "{", "someField", "=", "value", "}"
              , end " baz"
              ]

          it "accepts integer literals" $ do
            tokenize "\"foo \\{ bar { someField = 23 } } baz\"" `shouldBe` [
                begin "foo "
              , "bar", "{", "someField", "=", 23, "}"
              , end " baz"
              ]

          it "accepts arithmetic expressions" $ do
            tokenize "\"foo \\{ bar { someField = 23 + 42 } } baz\"" `shouldBe` [
                begin "foo "
              , "bar", "{", "someField", "=", 23, "+", 42, "}"
              , end " baz"
              ]

          it "accepts string literals" $ do
            tokenize "\"foo \\{ bar { someField = \"some string\" } } baz\"" `shouldBe` [
                begin "foo "
              , "bar", "{", "someField", "=", string "some string", "}"
              , end " baz"
              ]

          it "accepts interpolation in string literals" $ do
            tokenize "\"foo \\{ bar { someField = \"some \\{ 23 } string\" } } baz\"" `shouldBe` [
                begin "foo "
              , "bar", "{", "someField", "=", begin "some ", 23, end " string", "}"
              , end " baz"
              ]

          it "accepts list literals" $ do
            tokenize "\"foo \\{ bar { someField = [1, 2, 3] } } baz\"" `shouldBe` [
                begin "foo "
              , "bar", "{", "someField", "=", "[", 1, ",", 2, ",", 3, "]", "}"
              , end " baz"
              ]

          it "accepts list construction" $ do
            tokenize "\"foo \\{ bar { someField = x : xs} } baz\"" `shouldBe` [
                begin "foo "
              , "bar", "{", "someField", "=", "x", ":", "xs", "}"
              , end " baz"
              ]

          it "accepts nested record updates" $ do
            tokenize "\"\\{ foo { someField = bar { someOtherField = 23 } } }\"" `shouldBe` [
                begin ""
              , "foo", "{", "someField", "=", "bar", "{", "someOtherField", "=", 23, "}", "}"
              , end ""
              ]

    context "with errors" $ do
      let
        input = unlines [
            "foo :: Int -> Int"
          , "foo = \\ case"
          , "  23 -> 42"
          , "  _  -> 65"
          ]

      it "reports missing extensions" $ do
        (.errors) <$> Lexer.tokenize GHC2021 [] "main.hs" 1 input `shouldBe` Right "main.hs:2:9: error: [GHC-51179] Illegal \\case"

      it "takes provided extensions into account" $ do
        (.errors) <$> Lexer.tokenize GHC2021 [On LambdaCase] "main.hs" 1 input `shouldBe` Right ""

      it "takes LANGUAGE pragmas into account" $ do
        (.errors) <$> Lexer.tokenize GHC2021 [] "main.hs" 1 ("{-# LANGUAGE LambdaCase #-}\n" <> input) `shouldBe` Right ""

      it "takes negated LANGUAGE pragmas into account" $ do
        (.errors) <$> Lexer.tokenize GHC2021 [On LambdaCase] "main.hs" 1 ("{-# LANGUAGE NoLambdaCase #-}\n" <> input) `shouldBe` Right "main.hs:3:9: error: [GHC-51179] Illegal \\case"
