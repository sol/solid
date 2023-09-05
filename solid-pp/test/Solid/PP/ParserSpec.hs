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
import           GHC.Data.FastString (FastString)

import           Solid.PP (extensions)
import           Solid.PP.Lexer (Token(..))
import           Solid.PP.Parser hiding (parse)
import qualified Solid.PP.Parser as Parser

instance IsList (End () -> ExpressionWith ()) where
  type Item (End () -> ExpressionWith ()) = NodeWith ()
  fromList = Expression
  toList = undefined

instance IsList (Arguments ()) where
  type Item (Arguments ()) = NonEmpty (NodeWith ())
  fromList = Arguments () . map (Argument ())
  toList = undefined

instance Num (NodeWith ()) where
  (+) = undefined
  (-) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = token . fromInteger

instance IsString (NodeWith ()) where
  fromString name = nameWith (fromString name) NoArguments

instance IsString (Subject ()) where
  fromString name = Name () (fromString name) NoArguments

instance IsString (MethodCall ()) where
  fromString name = MethodCall () (fromString name) NoArguments

expectationFailurePure :: HasCallStack => String -> a
expectationFailurePure = throw . HUnitFailure (snd <$> callSite) . Reason

token :: Token -> NodeWith ()
token = Token ()

nameWith :: FastString -> Arguments () -> NodeWith ()
nameWith n args = MethodChain (Name () n args) []

parse :: HasCallStack => String -> [NodeWith ()]
parse = either expectationFailurePure (map void) . Parser.parse extensions "main.hs" 1 . fromString

spec :: Spec
spec = do
  let
    begin :: String -> ExpressionWith () -> NodeWith ()
    begin str expression = MethodChain (LiteralString $ Begin () str expression) []

    end :: String -> End ()
    end = End ()

    end_begin :: String -> ExpressionWith () -> End ()
    end_begin = EndBegin ()

    literal :: String -> NodeWith ()
    literal string = MethodChain (LiteralString (Literal () string)) []

    bracketed :: BracketStyle -> [[NodeWith ()]] -> NodeWith ()
    bracketed style inner = MethodChain (Bracketed style () inner) []

  describe "parse" $ do
    context "when parsing function calls" $ do
      it "accepts qualified names" $ do
        parse "String.foo(23)" `shouldBe` [MethodChain (QualifiedName () "String" "foo" [[23]]) []]

      context "when parsing arguments" $ do
        it "accepts a single argument" $ do
          parse "foo (23)" `shouldBe` ["foo", bracketed Round [[23]]]
          parse "foo(23)" `shouldBe` [nameWith "foo" [[23]]]

        it "accepts multiple arguments" $ do
          parse "foo (23, 42)" `shouldBe` ["foo", bracketed Round [[23], [42]]]
          parse "foo(23, 42)" `shouldBe` [nameWith "foo" [[23], [42]]]

        it "accepts nested tuples" $ do
          parse "foo((23, 42))" `shouldBe` [nameWith "foo" [[bracketed Round [[23], [42]]]]]

        it "accepts nested method calls" $ do
          parse "foo(bar(23))" `shouldBe` [nameWith "foo" [[nameWith "bar" [[23]]]]]

        it "accepts string literals" $ do
          parse "foo(\"Hey Joe👋\")" `shouldBe` [nameWith "foo" [[literal "\"Hey Joe👋\""]]]

    context "when parsing method chains" $ do
      it "accepts a name as the subject" $ do
        parse "foo.bar(23)" `shouldBe` [MethodChain "foo" [MethodCall () "bar" [[23]]]]

      it "accepts a qualified name as the subject" $ do
        parse "String.foo.bar(23)" `shouldBe` [MethodChain (QualifiedName () "String" "foo" NoArguments) [MethodCall () "bar" [[23]]]]

      it "accepts a function call as the subject" $ do
        parse "foo(bar).baz" `shouldBe` [MethodChain (Name () "foo" [["bar"]]) ["baz"]]

      it "accepts a method call as the subject" $ do
        parse "foo.bar(23).baz(42)" `shouldBe` [MethodChain "foo" [MethodCall () "bar" [[23]], MethodCall () "baz" [[42]]]]

      it "accepts a string literal as the subject" $ do
        parse "\"foo\".bar" `shouldBe` [MethodChain (LiteralString (Literal () "\"foo\"")) ["bar"]]

      it "accepts bracketed expressions as the subjects" $ do
        parse "(foo bar).baz" `shouldBe` [MethodChain (Bracketed Round () [["foo", "bar"]]) ["baz"]]

      it "properly handles bangs" $ do
        parse "foo!.bar!.baz!" `shouldBe` [MethodChain "foo!" ["bar!", "baz!"]]

    context "when parsing bracketed expressions" $ do
      it "accepts round brackets" $ do
        parse "(foo)" `shouldBe` [bracketed Round [["foo"]]]

      it "accepts square brackets" $ do
        parse "[foo]" `shouldBe` [bracketed Square [["foo"]]]

      it "accepts curly brackets" $ do
        parse "{foo}" `shouldBe` [bracketed Curly [["foo"]]]

      it "accepts an unapplied tuple constructor" $ do
        parse "(,,,)" `shouldBe` [bracketed Round [[],[],[],[]]]

    context "when parsing string literals" $ do
      it "accepts a literal string" $ do
        parse "\"foo\"" `shouldBe` [
            literal "\"foo\""
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
          Parser.parse extensions "main.hs" 1 "\"foo    \n" `shouldBe` Left "main.hs:1:9: error: [GHC-21231]\n    lexical error in string/character literal at character '\\n'"
          Parser.parse extensions "main.hs" 1 "\"foo {  \n" `shouldBe` Left "main.hs:1:9: error: [GHC-21231] lexical error at character '\\n'"

      context "on unexpected end of input" $ do
        it "reports an error" $ do
          Parser.parse extensions "main.hs" 1 "\"foo    " `shouldBe` Left "main.hs:1:9: error: [GHC-21231]\n    lexical error in string/character literal at end of input"
          let Left err = Parser.parse extensions "main.hs" 1 "\"foo {  "
          err `shouldBe` (unpack . unlines) [
              "main.hs:1:7:"
            , "  |"
            , "1 | \"foo {  "
            , "  |       ^"
            , "unterminated string interpolation"
            ]

      context "unexpected token" $ do
        it "reports an error" $ do
          let Left err = Parser.parse extensions "main.hs" 1 $ unlines [
                  "some tokens"
                , "bar ].foo"
                , "some more tokens"
                ]

          err `shouldBe` (unpack . unlines) [
              "main.hs:2:5:"
            , "  |"
            , "2 | bar ].foo"
            , "  |     ^"
            , "unexpected ]"
            , "expecting end of input"
            ]
