{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Solid.PP.ParserSpec (spec) where

import           Prelude ()
import           Solid.PP.IO

import           Test.Hspec
import           Test.HUnit.Lang
import           Solid.PP.LexerSpec ()
import           GHC.IsList
import           Data.CallStack (callSite)

import           Solid.PP (extensions)
import           Solid.PP.Lexer (Token(..))
import           Solid.PP.Parser hiding (parse)
import qualified Solid.PP.Parser as Parser

instance IsList (End () -> ExpressionWith ()) where
  type Item (End () -> ExpressionWith ()) = NodeWith ()
  fromList = Expression
  toList = undefined

instance IsString (NodeWith ()) where
  fromString = token . fromString

expectationFailurePure :: HasCallStack => String -> a
expectationFailurePure = throw . HUnitFailure (snd <$> callSite) . Reason

token :: Token -> NodeWith ()
token = Token ()

parse :: HasCallStack => String -> [NodeWith ()]
parse = either expectationFailurePure (map void) . Parser.parse extensions "main.hs" . fromString

spec :: Spec
spec = do
  let
    begin = fmap LiteralString . Begin ()
    end = End ()
    end_begin = EndBegin ()

  describe "parse" $ do
    context "when parsing string literals" $ do
      it "accepts a literal string" $ do
        parse "\"foo\"" `shouldBe` [
            LiteralString (Literal () "\"foo\"")
          ]

      it "accepts a string with interpolation" $ do
        parse "\"foo {bar} baz\"" `shouldBe` [
            begin "\"foo {" $ ["bar"] $ end "} baz\""
          ]

      it "accepts a string with multiple interpolations" $ do
        parse "\" {foo} {bar} {baz} \"" `shouldBe` [
            begin "\" {" $ ["foo"] $ end_begin "} {" $ ["bar"] $ end_begin "} {" $ ["baz"] $ end "} \""
          ]

      it "accepts a string with nested interpolations" $ do
        parse "\" { \" {foo} \" } \"" `shouldBe` [
            begin "\" {" $ [begin "\" {" $ ["foo"] $ end "} \""] $ end "} \""
          ]

      context "on unexpected end of line" $ do
        it "reports an error" $ do
          Parser.parse extensions "main.hs" "\"foo    \n" `shouldBe` Left "main.hs:1:9: error: [GHC-21231]\n    lexical error in string/character literal at character '\\n'"
          Parser.parse extensions "main.hs" "\"foo {  \n" `shouldBe` Left "main.hs:1:9: error: [GHC-21231] lexical error at character '\\n'"

      context "on unexpected end of input" $ do
        it "reports an error" $ do
          Parser.parse extensions "main.hs" "\"foo    " `shouldBe` Left "main.hs:1:9: error: [GHC-21231]\n    lexical error in string/character literal at end of input"
          let Left err = Parser.parse extensions "main.hs" "\"foo {  "
          err `shouldBe` (unpack . unlines) [
              "main.hs:1:7:"
            , "  |"
            , "1 | \"foo {  "
            , "  |       ^"
            , "unterminated string interpolation"
            ]
