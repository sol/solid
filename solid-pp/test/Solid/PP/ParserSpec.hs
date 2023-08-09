{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.PP.ParserSpec (spec) where

import           Prelude ()
import           Solid.PP.IO

import           Test.Hspec
import           Solid.PP.LexerSpec ()
import           GHC.IsList

import           Solid.PP (extensions)
import           Solid.PP.Parser hiding (parse)
import qualified Solid.PP.Parser as Parser

instance IsList (End () -> ExpressionWith ()) where
  type Item (End () -> ExpressionWith ()) = NodeWith ()
  fromList = Expression
  toList = undefined

parse :: String -> [NodeWith ()]
parse = either error (map void) . Parser.parse extensions "" . fromString

spec :: Spec
spec = do
  let
    begin = fmap LiteralString . Begin ()
    end = End ()
    end_begin = EndBegin ()
    token = Token ()

  describe "parse" $ do
    it "accepts a literal string" $ do
      parse "\"foo\"" `shouldBe` [
          LiteralString (Literal () "\"foo\"")
        ]

    it "accepts a string with interpolation" $ do
      parse "\"foo {bar} baz\"" `shouldBe` [
          begin "\"foo {" $ [token "bar"] $ end "} baz\""
        ]

    it "accepts a string with multiple interpolations" $ do
      parse "\" {foo} {bar} {baz} \"" `shouldBe` [
          begin "\" {" $ [token "foo"] $ end_begin "} {" $ [token "bar"] $ end_begin "} {" $ [token "baz"] $ end "} \""
        ]

    it "accepts a string with nested interpolations" $ do
      parse "\" { \" {foo} \" } \"" `shouldBe` [
          begin "\" {" $ [begin "\" {" $ [token "foo"] $ end "} \""] $ end "} \""
        ]

    context "on unexpected end of line" $ do
      it "reports an error" $ do
        Parser.parse extensions "main.hs" "\"foo    \n" `shouldBe` Left "main.hs:1:9: error: [GHC-21231]\n    lexical error in string/character literal at character '\\n'"
        Parser.parse extensions "main.hs" "\"foo {  \n" `shouldBe` Left "main.hs:1:9: error: [GHC-21231] lexical error at character '\\n'"

    context "on unexpected end of input" $ do
      it "reports an error" $ do
        Parser.parse extensions "main.hs" "\"foo    " `shouldBe` Left "main.hs:1:9: error: [GHC-21231]\n    lexical error in string/character literal at end of input"
        Parser.parse extensions "main.hs" "\"foo {  " `shouldBe` Left "main.hs:1:9: unterminated string interpolation"
