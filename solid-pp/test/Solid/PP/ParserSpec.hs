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
import           GHC.Data.FastString (FastString, mkFastString)
import qualified Data.Text as T
import           Data.Char
import qualified Data.List as List

import           Solid.PP (language, extensions)
import           Solid.PP.Lexer (Token(..))
import qualified Solid.PP.Lexer as Lexer
import           Solid.PP.Parser hiding (parseModule)
import qualified Solid.PP.Parser as Parser
import           Solid.PP.SrcLoc

instance IsList (Module ()) where
  type Item (Module ()) = Node ()
  fromList = Module NoModuleHeader []
  toList = undefined

instance IsList (ImportExportItems ()) where
  type Item (ImportExportItems ()) = [Node ()]
  fromList = ImportExportItems
  toList = undefined

instance IsString (MethodName ()) where
  fromString = MethodName () . fromString

instance IsString (Type ()) where
  fromString t = case t of
    c : _ | isLower c -> TypeVariable (fromString t)
    _ -> TypeName () Nothing (fromString t)

instance IsString (ModuleHeader ()) where
  fromString name = ModuleHeader () (fromString name) NoExportList

instance IsString (ModuleName ()) where
  fromString string = case splitModuleName string of
    [] -> ModuleName () Nothing ""
    name : qualified -> ModuleName () (joinModuleName qualified) (packFS name)
    where
      packFS = mkFastString . T.unpack
      splitModuleName = reverse . T.splitOn "." . T.pack
      joinModuleName qualified = case reverse qualified of
        [] -> Nothing
        parts -> Just . packFS $ T.intercalate "." parts

instance IsString (ImportName ()) where
  fromString = ImportName NoPackageName . fromString

instance IsString (PackageName ()) where
  fromString = PackageName () . fromString

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
  fromString = \ case
    "=" -> token ITequal
    name -> nameWith (fromString name) NoArguments

instance IsString (Subject ()) where
  fromString name = Name () (fromString name) NoArguments

instance IsString (MethodCall ()) where
  fromString name = MethodCall () (fromString name) NoArguments

token :: Token -> Node ()
token = Token ()

nameWith :: FastString -> Arguments () -> Node ()
nameWith n args = MethodChain (Name () n args) []

parseModule :: Text -> Either String (Module BufferSpan)
parseModule input = Parser.parseModule language extensions (Original "main.hs") (InputFile "main.hs" input)

parse :: HasCallStack => Text -> (Module () -> Expectation) -> Expectation
parse input action = annotated $ do
  either assertFailure (return . void) (parseModule input) >>= action
  where
    annotated :: IO a -> IO a
    annotated = either (const id) (annotate . formatTokens) $ Lexer.tokenize language extensions "main.hs" 1 input

    formatTokens :: Lexer.LexerResult -> String
    formatTokens result = "\ESC[31mtokens: \ESC[36m" <> List.intercalate "  " (map formatToken result.tokens) <> "\ESC[39m"

    formatToken :: WithBufferSpan Token -> String
    formatToken = show . unLoc

infix 1 `shouldBe`, `shouldParseAs`

shouldBe :: HasCallStack => (Eq a, Show a) => ((a -> Expectation) -> Expectation) -> a -> Expectation
shouldBe action a = action (`Hspec.shouldBe` a)

shouldParseAs :: HasCallStack => Text -> Module () -> Expectation
shouldParseAs input expected = parse input `shouldBe` expected

spec :: Spec
spec = do
  let
    begin :: FastString -> Expression () -> Node ()
    begin str expression = MethodChain (LiteralString $ Begin () str expression) []

    end :: FastString -> End ()
    end = End ()

    end_begin :: FastString -> Expression () -> End ()
    end_begin = EndBegin ()

    literal :: FastString -> Node ()
    literal string = MethodChain (LiteralString (Literal () string)) []

    bracketed :: [[Node ()]] -> Node ()
    bracketed inner = MethodChain (Bracketed () inner) []

  describe "parse" $ do
    context "when parsing pragmas" $ do
      it "accepts pragmas" $ do
        parse "{-# COMPLETE Foo #-}" `shouldBe` [Pragma () [token (ITconid "Foo")]]

      it "accepts comma-separated arguments" $ do
        parse "{-# COMPLETE Foo, Bar #-}" `shouldBe` [Pragma () [token (ITconid "Foo"), token ITcomma, token (ITconid "Bar")]]

      it "accepts partial pragmas" $ do
        parse "{-# COMPLETE Foo, " `shouldBe` [Pragma () [token (ITconid "Foo"), token ITcomma]]

    context "when parsing module headers" $ do
      it "accepts an unqualified module name" $ do
        parse "module Foo where" `shouldBe` Module "Foo" [] []

      it "accepts a qualified module name" $ do
        parse "module Foo.Bar where" `shouldBe` Module "Foo.Bar" [] []

      it "accepts an export list" $ do
        parse "module Foo (bar, baz) where" `shouldBe` Module (ModuleHeader () "Foo" (ExportList [["bar"], ["baz"]])) [] []

    context "when parsing use statements" $ do
      it "accepts use-statements" $ do
        parse "use Data.Foldable" `shouldBe` Module NoModuleHeader [Import () (Use NoUseWith) "Data.Foldable" Nothing NoImportList] []

      it "accepts use-with-statements" $ do
        parse "use Data.Foldable as Foo (foo, bar) with (baz)" `shouldBe` Module NoModuleHeader [Import () (Use $ UseWith () [["baz"]]) "Data.Foldable" (Just "Foo") (ImportList [["foo"], ["bar"]])] []

    context "when parsing imports" $ do
      it "accepts imports" $ do
        parse (unlines [
            "module Foo where"
          , "import Bar"
          , "import Baz"
          ]) `shouldBe` Module "Foo" ["Bar", "Baz"] []

      it "accepts package imports" $ do
        parse "import \"foo\" Foo" `shouldBe` Module NoModuleHeader [Import () Unqualified (ImportName "foo" "Foo") Nothing NoImportList] []

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

    context "when parsing method definitions" $ do
      let method name ctx args subject result = MethodDefinition $ Method () name WithoutStackTrace ctx args subject result () ()

      it "parses method definitions" $ do
        unlines [
            ".length :: String -> Int"
          , ".length = coerce utf8length"
          ]
        `shouldParseAs` [method "length" [] [] "String" "Int", "=", "coerce", "utf8length"]

      it "accepts list types" $ do
        unlines [
            ".words :: String -> [String]"
          , ".words = coerce utf8words"
          ]
        `shouldParseAs` [method "words" [] [] "String" (ListOf "String"), "=", "coerce", "utf8words"]

      it "accepts tuple types" $ do
        unlines [
            ".foo :: S -> (A, B, C)"
          , ".foo = bar"
          ]
        `shouldParseAs` [method "foo" [] [] "S" (Tuple ["A", "B", "C"]), "=", "bar"]

      it "accepts methods with arity greater 1" $ do
        unlines [
            ".foo :: A -> B -> Subject -> Result"
          , ".foo = undefined"
          ]
        `shouldParseAs` [method "foo" [] ["A", "B"] "Subject" "Result", "=", "undefined"]

      it "accepts the unit type" $ do
        unlines [
            ".remove :: FilePath -> IO ()"
          , ".remove = undefined"
          ]
        `shouldParseAs` [method "remove" [] [] "FilePath" (TypeApplication "IO" (Tuple [])), "=", "undefined"]

      it "accepts type applications" $ do
        unlines [
            ".foo :: FilePath -> Either Int String"
          , ".foo = undefined"
          ]
        `shouldParseAs` [method "foo" [] [] "FilePath" (TypeApplication (TypeApplication "Either" "Int") "String"), "=", "undefined"]

      it "accepts qualified type names" $ do
        unlines [
            ".open :: IO.Mode -> FilePath -> IO Handle"
          , ".open = undefined"
          ]
        `shouldParseAs` [method "open" [] [TypeName () (Just "IO") "Mode"] "FilePath" (TypeApplication "IO" "Handle"), "=", "undefined"]

      it "accepts type constraints" $ do
        unlines [
            ".foo? :: Eq a => Maybe a -> Bool"
          , ".foo? = isJust"
          ]
        `shouldParseAs` [method "foo?" [TypeApplication "Eq" "a"] [] (TypeApplication "Maybe" "a") "Bool", "=", "isJust"]


    context "when parsing function calls" $ do
      it "accepts qualified names" $ do
        parse "String.foo(23)" `shouldBe` [MethodChain (QualifiedName () "String" "foo" [[23]]) []]

      context "when parsing arguments" $ do
        it "accepts a single argument" $ do
          parse "foo (23)" `shouldBe` ["foo", bracketed [[23]]]
          parse "foo(23)" `shouldBe` [nameWith "foo" [[23]]]

        it "accepts multiple arguments" $ do
          parse "foo (23, 42)" `shouldBe` ["foo", bracketed [[23], [42]]]
          parse "foo(23, 42)" `shouldBe` [nameWith "foo" [[23], [42]]]

        it "accepts nested tuples" $ do
          parse "foo((23, 42))" `shouldBe` [nameWith "foo" [[bracketed [[23], [42]]]]]

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
        parse "(foo bar).baz" `shouldBe` [MethodChain (Bracketed () [["foo", "bar"]]) ["baz"]]

      it "properly handles bangs" $ do
        parse "foo!.bar!.baz!" `shouldBe` [MethodChain "foo!" ["bar!", "baz!"]]

    context "when parsing bracketed expressions" $ do
      it "accepts round brackets" $ do
        parse "(foo)" `shouldBe` [bracketed [["foo"]]]

      it "accepts square brackets" $ do
        parse "[foo]" `shouldBe` [bracketed [["foo"]]]

      it "accepts curly brackets" $ do
        parse "{foo}" `shouldBe` [bracketed [["foo"]]]

      it "accepts an unapplied tuple constructor" $ do
        parse "(,,,)" `shouldBe` [bracketed [[],[],[],[]]]

      it "accepts unboxed tuples" $ do
        unlines [
            "{-# LANGUAGE UnboxedTuples #-}"
          , "(# 23, 42 #)"
          ]
        `shouldParseAs` [bracketed [[23], [42]]]

      it "accepts TemplateHaskell expression quotes" $ do
        parse "[| foo bar 23 |]" `shouldBe` [bracketed [["foo", "bar", 23]]]

      it "accepts typed TemplateHaskell expression quotes" $ do
        parse "[|| foo bar 23 ||]" `shouldBe` [bracketed [["foo", "bar", 23]]]

      it "accepts TemplateHaskell pattern quotes" $ do
        unlines [
            "{-# LANGUAGE TemplateHaskell #-}"
          , "foo = [p|(x, y)|]"
          ]
        `shouldParseAs` ["foo", "=", bracketed [[bracketed[["x"], ["y"]]]]]

      it "accepts TemplateHaskell definition quotes" $ do
        unlines [
            "{-# LANGUAGE TemplateHaskell #-}"
          , "foo = [d|data Foo|]"
          ]
        `shouldParseAs` ["foo", "=", bracketed [[token ITvocurly, token ITdata, token (ITconid "Foo")]], token ITvccurly]

      it "accepts TemplateHaskell type quotes" $ do
        unlines [
            "{-# LANGUAGE TemplateHaskell #-}"
          , "foo = [t|Maybe|]"
          ]
        `shouldParseAs` ["foo", "=", bracketed [[token (ITconid "Maybe")]]]

      context "at the end of input" $ do
        it "ignores missing closing brackets" $ do
          parse "(foo bar " `shouldBe` [bracketed [["foo", "bar"]]]

      context "on mismatching close" $ do
        it "produces useful error messages" $ do
          let Left err = parseModule "[|| foo bar 23 |]"
          err `Hspec.shouldBe` List.intercalate "\n" [
              "main.hs:1:16: unexpected |]"
            , " expecting ||]"
            ]

    context "when parsing string literals" $ do
      it "accepts a literal string" $ do
        parse "\"foo\"" `shouldBe` [
            literal "\"foo\""
          ]

      it "accepts a string with interpolation" $ do
        parse "\"foo \\{bar} baz\"" `shouldBe` [
            begin "\"foo \\{" $ ["bar"] $ end "} baz\""
          ]

      it "accepts a string with multiple interpolations" $ do
        parse "\" \\{foo} \\{bar} \\{baz} \"" `shouldBe` [
            begin "\" \\{" $ ["foo"] $ end_begin "} \\{" $ ["bar"] $ end_begin "} \\{" $ ["baz"] $ end "} \""
          ]

      it "accepts a string with nested interpolations" $ do
        parse "\" \\{ \" \\{foo} \" } \"" `shouldBe` [
            begin "\" \\{" $ [begin "\" \\{" $ ["foo"] $ end "} \""] $ end "} \""
          ]

      context "on unexpected end of line" $ do
        it "reports an error" $ do
          parseModule "\"foo    \n" `Hspec.shouldBe` Left "main.hs:1:9: error: [GHC-21231]\n    lexical error in string/character literal at character '\\n'"
          parseModule "\"foo \\{  \n" `Hspec.shouldBe` Left "main.hs:1:10: error: [GHC-21231] lexical error at character '\\n'"

      context "on unexpected end of input" $ do
        it "reports an error" $ do
          parseModule "\"foo    " `Hspec.shouldBe` Left "main.hs:1:9: error: [GHC-21231]\n    lexical error in string/character literal at end of input"
          let Left err = parseModule "\"foo \\{  "
          err `Hspec.shouldBe` "main.hs:1:8:unterminated string interpolation"

      context "unexpected token" $ do
        it "reports an error" $ do
          let Left err = parseModule $ unlines [
                  "some tokens"
                , "bar ].foo"
                , "some more tokens"
                ]
          err `Hspec.shouldBe` "main.hs:2:5:unexpected ]"
