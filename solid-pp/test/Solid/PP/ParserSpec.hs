{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Solid.PP.ParserSpec (spec) where

import           Prelude ()
import           Solid.PP.IO

import           Test.Hspec hiding (shouldBe)
import qualified Test.Hspec as Hspec
import           Test.Hspec.Expectations.Contrib (annotate)
import           Test.HUnit.Lang
import           Solid.PP.LexerSpec ()
import           GHC.IsList
import           GHC.Data.FastString (FastString)

import           Solid.PP (extensions)
import           Solid.PP.Lexer (Token(..))
import qualified Solid.PP.Lexer as Lexer
import           Solid.PP.Parser
import qualified Solid.PP.Parser as Parser
import           Solid.PP.SrcLoc

instance IsList (Module ()) where
  type Item (Module ()) = Node ()
  fromList = Module NoModuleHeader []
  toList = undefined

instance IsString (ModuleHeader ()) where
  fromString name = ModuleHeader () (fromString name) NoExportList

instance IsString (ImportName ()) where
  fromString = ImportName Nothing . fromString

instance IsString (Import ()) where
  fromString name = Import () Unqualified (fromString name) Nothing NoImportList

instance IsList (End () -> Expression ()) where
  type Item (End () -> Expression ()) = Node ()
  fromList = Expression
  toList = undefined

instance IsList (Arguments ()) where
  type Item (Arguments ()) = NonEmpty (Node ())
  fromList = Arguments () . map (Argument ())
  toList = undefined

instance Num (Node ()) where
  (+) = undefined
  (-) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = token . fromInteger

instance IsString (Node ()) where
  fromString name = nameWith (fromString name) NoArguments

instance IsString (Subject ()) where
  fromString name = Name () (fromString name) NoArguments

instance IsString (MethodCall ()) where
  fromString name = MethodCall () (fromString name) NoArguments

token :: Token -> Node ()
token = Token ()

nameWith :: FastString -> Arguments () -> Node ()
nameWith n args = MethodChain (Name () n args) []

parse :: HasCallStack => Text -> (Module () -> Expectation) -> Expectation
parse input action = annotated $ do
  either assertFailure (return . void) (Parser.parseModule extensions "main.hs" 1 input) >>= action
  where
    annotated :: IO a -> IO a
    annotated = either (const id) (annotate . formatTokens) $ Lexer.tokenize extensions "main.hs" 1 input

    formatTokens :: Lexer.LexerResult -> String
    formatTokens result = "\ESC[31mtokens: \ESC[36m" <> show (map unLoc result.tokens) <> "\ESC[39m"

shouldBe :: HasCallStack => (Eq a, Show a) => ((a -> Expectation) -> Expectation) -> a -> Expectation
shouldBe action a = action (`Hspec.shouldBe` a)

spec :: Spec
spec = do
  let
    begin :: String -> Expression () -> Node ()
    begin str expression = MethodChain (LiteralString $ Begin () str expression) []

    end :: String -> End ()
    end = End ()

    end_begin :: String -> Expression () -> End ()
    end_begin = EndBegin ()

    literal :: String -> Node ()
    literal string = MethodChain (LiteralString (Literal () string)) []

    bracketed :: BracketStyle -> [[Node ()]] -> Node ()
    bracketed style inner = MethodChain (Bracketed style () inner) []

  describe "parse" $ do
    context "when parsing module headers" $ do
      it "accepts an unqualified module name" $ do
        parse "module Foo where" `shouldBe` Module "Foo" [] []

      it "accepts a qualified module name" $ do
        parse "module Foo.Bar where" `shouldBe` Module "Foo.Bar" [] []

      it "accepts an export list" $ do
        parse "module Foo (bar, baz) where" `shouldBe` Module (ModuleHeader () "Foo" (ExportList [["bar"], ["baz"]])) [] []

    context "when parsing imports" $ do
      it "accepts imports" $ do
        parse (unlines [
            "module Foo where"
          , "import Bar"
          , "import Baz"
          ]) `shouldBe` Module "Foo" ["Bar", "Baz"] []

      it "accepts package imports" $ do
        parse "import \"foo\" Foo" `shouldBe` Module NoModuleHeader [Import () Unqualified (ImportName (Just "foo") "Foo") Nothing NoImportList] []

      it "accepts renamed imports" $ do
        parse "import Foo as Bar" `shouldBe` Module NoModuleHeader [Import () Unqualified "Foo" (Just "Bar") NoImportList] []

      it "accepts qualified imports" $ do
        parse "import qualified Foo" `shouldBe` Module NoModuleHeader [Import () Qualified "Foo" Nothing NoImportList] []

      it "accepts post-qualified imports" $ do
        parse "import Foo qualified as Bar" `shouldBe` Module NoModuleHeader [Import () QualifiedPost "Foo" (Just "Bar") NoImportList] []

      it "accepts import lists" $ do
        parse "import Foo (bar, baz)" `shouldBe` Module NoModuleHeader [Import () Unqualified "Foo" Nothing (ImportList [["bar"], ["baz"]])] []

      it "accepts hiding lists" $ do
        parse "import Foo hiding (bar, baz)" `shouldBe` Module NoModuleHeader [Import () Unqualified "Foo" Nothing (HidingList [["bar"], ["baz"]])] []

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
          Parser.parseModule extensions "main.hs" 1 "\"foo    \n" `Hspec.shouldBe` Left "main.hs:1:9: error: [GHC-21231]\n    lexical error in string/character literal at character '\\n'"
          Parser.parseModule extensions "main.hs" 1 "\"foo {  \n" `Hspec.shouldBe` Left "main.hs:1:9: error: [GHC-21231] lexical error at character '\\n'"

      context "on unexpected end of input" $ do
        it "reports an error" $ do
          Parser.parseModule extensions "main.hs" 1 "\"foo    " `Hspec.shouldBe` Left "main.hs:1:9: error: [GHC-21231]\n    lexical error in string/character literal at end of input"
          let Left err = Parser.parseModule extensions "main.hs" 1 "\"foo {  "
          err `Hspec.shouldBe` (unpack . unlines) [
              "main.hs:1:7:"
            , "  |"
            , "1 | \"foo {  "
            , "  |       ^"
            , "unterminated string interpolation"
            ]

      context "unexpected token" $ do
        it "reports an error" $ do
          let Left err = Parser.parseModule extensions "main.hs" 1 $ unlines [
                  "some tokens"
                , "bar ].foo"
                , "some more tokens"
                ]

          err `Hspec.shouldBe` (unpack . unlines) [
              "main.hs:2:5:"
            , "  |"
            , "2 | bar ].foo"
            , "  |     ^"
            , "unexpected ]"
            , "expecting end of input"
            ]
