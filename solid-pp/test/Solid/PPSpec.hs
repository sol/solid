{-# LANGUAGE OverloadedStrings #-}
module Solid.PPSpec (spec) where

import           Prelude ()
import           Solid.PP.IO

import           Test.Hspec
import           Test.Mockery.Directory
import qualified Data.Set as Set
import qualified Data.List as List

import           Solid.PP.Parser

import           Solid.PP

infix 1 `shouldDesugarTo`

run_ :: HasCallStack => FilePath -> FilePath -> FilePath -> IO ()
run_ src cur dst = run src cur dst >>= \ case
  Failure message -> expectationFailure message
  Success -> pass

shouldDesugarTo :: HasCallStack => Text -> Text -> Expectation
shouldDesugarTo input expected = do
  let file = "main.hs"
  writeFile file input
  run_ file file file
  readFile file `shouldReturn` "{-# LINE 1 " <> pack (show file) <> " #-}\n" <> expected

interpolationShouldDesugarTo :: HasCallStack => Text -> Text -> Expectation
interpolationShouldDesugarTo input expected = do
  let file = "main.hs"
  writeFile file input
  Success <- run file file file
  readFile file `shouldReturn`
       "{-# LINE 1 " <> pack (show file) <> " #-}\n"
    <> "import qualified Solid.ToString\n"
    <> "{-# LINE 1 " <> pack (show file) <> " #-}\n"
    <> expected

spec :: Spec
spec = do
  describe "implicitImport" $ do
    context "with a module name" $ do
      it "constructs an ImplicitImport value" $ do
        implicitImport (ModuleName () Nothing "Foo") `shouldBe` ImplicitImport "Foo"

    context "with a hierarchical module name" $ do
      it "constructs an ImplicitImport value" $ do
        implicitImport (ModuleName () (Just "Foo") "Bar") `shouldBe` ImplicitImport "Foo.Bar"

  describe "implicitImports" $ do
    let
      modules :: HasCallStack => Text -> ImplicitImports
      modules input = implicitImports . either error id $ parseModule extensions (InputFile "src.hs" input) (InputFile "src.hs" input)

    context "with a qualified identifier" $ do
      it "extracts module name" $ do
        modules "foo = String.length" `shouldBe` Set.fromList ["String"]

    context "within a function argument" $ do
      it "extracts module names" $ do
        modules "foo(String.length bar)" `shouldBe` Set.fromList ["String"]

    context "within an export list" $ do
      it "extracts module names" $ do
        modules "module Foo (String.length) where" `shouldBe` Set.fromList ["String"]

    context "with a qualified constructor" $ do
      it "extracts module name" $ do
        modules "foo = IO.ReadMode" `shouldBe` Set.fromList ["IO"]

    context "with a qualified type" $ do
      it "extracts module name" $ do
        modules "foo :: IO.Mode" `shouldBe` Set.fromList ["IO"]

    context "within an interpolated string" $ do
      it "extracts module names" $ do
        modules "foo \"some {Foo.x} test {Bar.x} input\"" `shouldBe` Set.fromList ["Solid.ToString", "Bar", "Foo"]

    context "when a qualified name references the current module" $ do
      it "does not extract that module name" $ do
        modules (unlines [
            "module Foo where"
          , "foo = Foo.bar"
          ]) `shouldBe` Set.fromList []

    context "with imports" $ do
      it "does not filter out qualified imports" $ do
        modules (unlines [
            "import qualified String"
          , "foo = String.length"
          ]) `shouldBe` Set.fromList ["String"]

      it "does not filter out post-qualified imports" $ do
        modules (unlines [
            "import String qualified"
          , "foo = String.length"
          ]) `shouldBe` Set.fromList ["String"]

      it "filters out imports with an import list" $ do
        modules (unlines [
            "import qualified String (length)"
          , "foo = String.length"
          ]) `shouldBe` Set.fromList []

      it "filters out unqualified imports" $ do
        modules (unlines [
            "import String"
          , "foo = String.length"
          ]) `shouldBe` Set.fromList []

      it "filters out renamed imports" $ do
        modules (unlines [
            "import qualified Text as String"
          , "foo = String.length"
          ]) `shouldBe` Set.fromList []

    context "with use-statements" $ do
      it "does not filter out use-statements" $ do
        modules (unlines [
            "use String"
          , "foo = String.length"
          ]) `shouldBe` Set.fromList ["String"]

      it "filters out hierarchical use-statements" $ do
        modules (unlines [
            "use Data.String"
          , "foo = String.length"
          ]) `shouldBe` Set.fromList []

      it "filters out use-statements with an import list" $ do
        modules (unlines [
            "use String (length)"
          , "foo = String.length"
          ]) `shouldBe` Set.fromList []

      it "filters out renamed use-statements" $ do
        modules (unlines [
            "use Data.Text as String"
          , "foo = String.length"
          ]) `shouldBe` Set.fromList []

  describe "desugarExpression" $ around_ inTempDirectory $ do
    it "desugars identifiers" $ do
      desugarExpression "src.hs" 1 "foo!" `shouldBe` Right "fooᴉ"

    it "desugars string literals" $ do
      desugarExpression "src.hs" 1 "\"foo {23} bar\"" `shouldBe` Right "(\"foo \" <> Solid.ToString.toString ({-# COLUMN 7 #-}23) <> \" bar\"{-# COLUMN 14 #-})"

  describe "run" $ around_ inTempDirectory $ do
    context "when lexing fails" $ do
      it "reports error locations" $ do
        writeFile "src.hs" $ unlines [
            "foo :: Int"
          , "  {-"
          , "foo = 23"
          ]
        run "src.hs" "src.hs" "dst.hs" `shouldReturn` Failure "src.hs:2:3: error: [GHC-21231] unterminated `{-' at end of input"

      it "takes LINE pragmas into account" $ do
        writeFile "src.hs" $ unlines [
            "{-# LINE 23 \"foo.hs\" #-}"
          , "foo :: Int"
          , "{-"
          , "foo = 23"
          ]
        run "src.hs" "src.hs" "dst.hs" `shouldReturn` Failure "foo.hs:24:1: error: [GHC-21231] unterminated `{-' at end of input"

      it "does not report failures for GHC2021 syntax" $ do
        writeFile "src.hs" $ unlines [
            "foo :: Int"
          , "foo = 23_0"
          , "{-"
          ]
        run "src.hs" "src.hs" "dst.hs" `shouldReturn` Failure "src.hs:3:1: error: [GHC-21231] unterminated `{-' at end of input"

    context "when parsing fails" $ do
      it "reports error locations" $ do
        writeFile "src.hs" $ unlines [
            "module Foo where"
          , ""
          , "foo , bar"
          ]
        run "src.hs" "src.hs" "dst.hs" `shouldReturn` Failure (List.unlines [
            "src.hs:3:5:"
          , "  |"
          , "3 | foo , bar"
          , "  |     ^"
          , "unexpected ,"
          , "expecting end of input"
          ])

      it "takes LINE pragmas into account" $ do
        writeFile "src.hs" $ unlines [
            "module Foo where"
          , ""
          , "foo = ("
          ]
        writeFile "cur.hs" $ unlines [
            "module Foo where"
          , ""
          , ""
          , ""
          , "{-# LINE 3 \"src.hs\" #-}"
          , "foo = ("
          ]
        run "src.hs" "cur.hs" "dst.hs" `shouldReturn` Failure (List.unlines [
            "src.hs:3:8:"
          , "  |"
          , "3 | foo = ("
          , "  |        ^"
          , "unexpected end of input"
          ])

    context "when pre-processing imports" $ do
      it "implicitly imports well-know modules" $ do
        writeFile "src.hs" $ unlines [
            "foo :: String -> Int"
          , "foo = String.length"
          ]
        run_ "src.hs" "src.hs" "dst.hs"
        readFile "dst.hs" `shouldReturn` unlines [
            "{-# LINE 1 \"src.hs\" #-}"
          , "import qualified String"
          , "{-# LINE 1 \"src.hs\" #-}"
          , "foo :: String -> Int"
          , "foo = String.length"
          ]

      context "with existing imports" $ do
        it "places implicit imports before any existing imports" $ do
          writeFile "src.hs" $ unlines [
              "import qualified Foo"
            , "foo :: String -> Int"
            , "foo = String.length"
            ]
          run_ "src.hs" "src.hs" "dst.hs"
          readFile "dst.hs" `shouldReturn` unlines [
              "{-# LINE 1 \"src.hs\" #-}"
            , "import qualified String"
            , "{-# LINE 1 \"src.hs\" #-}"
            , "import qualified Foo"
            , "foo :: String -> Int"
            , "foo = String.length"
            ]

      context "with LANGUAGE pragmas" $ do
        it "places implicit imports after the last pragma" $ do
          writeFile "src.hs" $ unlines [
              "-- some comment"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , "{-# OPTIONS_GHC -fno-warn-orphans #-}"
            , "foo :: String -> Int"
            , "{-# INLINE foo #-}"
            , "foo = String.length"
            ]
          run_ "src.hs" "src.hs" "dst.hs"
          readFile "dst.hs" `shouldReturn` unlines [
              "{-# LINE 1 \"src.hs\" #-}"
            , "-- some comment"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , "{-# OPTIONS_GHC -fno-warn-orphans #-}"
            , "import qualified String"
            , "{-# LINE 4 \"src.hs\" #-}"
            , "foo :: String -> Int"
            , "{-# INLINE foo #-}"
            , "foo = String.length"
            ]

        context "without a module body" $ do
          it "does not implicitly import anything" $ do
            writeFile "src.hs" $ unlines [
                "-- some comment"
              , "{-# LANGUAGE OverloadedStrings #-}"
              , "{-# OPTIONS_GHC -fno-warn-orphans #-}"
              ]
            run_ "src.hs" "src.hs" "dst.hs"
            readFile "dst.hs" `shouldReturn` unlines [
                "{-# LINE 1 \"src.hs\" #-}"
              , "-- some comment"
              , "{-# LANGUAGE OverloadedStrings #-}"
              , "{-# OPTIONS_GHC -fno-warn-orphans #-}"
              ]

      context "with a module header" $ do
        it "places implicit imports after the module header" $ do
          writeFile "src.hs" $ unlines [
              "{-# LANGUAGE OverloadedStrings #-}"
            , "module Foo where"
            , "foo :: String -> Int"
            , "foo = String.length"
            ]
          run_ "src.hs" "src.hs" "dst.hs"
          readFile "dst.hs" `shouldReturn` unlines [
              "{-# LINE 1 \"src.hs\" #-}"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , "module Foo where"
            , "import qualified String"
            , "{-# LINE 2 \"src.hs\" #-}"
            , ""
            , "foo :: String -> Int"
            , "foo = String.length"
            ]

        it "does not implicitly import itself" $ do
          writeFile "src.hs" $ unlines [
              "module String where"
            , "foo = String.length"
            ]
          run_ "src.hs" "src.hs" "dst.hs"
          readFile "dst.hs" `shouldReturn` unlines [
              "{-# LINE 1 \"src.hs\" #-}"
            , "module String where"
            , "foo = String.length"
            ]

        context "without any qualified names" $ do
          it "does not modify anything" $ do
            writeFile "src.hs" $ unlines [
                "module Foo where"
              , "foo :: Int"
              , "foo = 23"
              ]
            run_ "src.hs" "src.hs" "dst.hs"
            readFile "dst.hs" `shouldReturn` unlines [
                "{-# LINE 1 \"src.hs\" #-}"
              , "module Foo where"
              , "foo :: Int"
              , "foo = 23"
              ]

        context "with an export list" $ do
          it "places implicit imports after the module header" $ do
            writeFile "src.hs" $ unlines [
                "module Foo (foo!) where"
              , "foo! :: String -> Int"
              , "foo! = String.length"
              ]
            run_ "src.hs" "src.hs" "dst.hs"
            readFile "dst.hs" `shouldReturn` unlines [
                "{-# LINE 1 \"src.hs\" #-}"
              , "module Foo (fooᴉ) where"
              , "import qualified String"
              , "{-# LINE 1 \"src.hs\" #-}"
              , ""
              , "fooᴉ :: String -> Int"
              , "fooᴉ = String.length"
              ]

          context "with an empty module body" $ do
            it "places implicit imports after the module header" $ do
              writeFile "src.hs" $ unlines [
                  "module Foo (String.length) where"
                ]
              run_ "src.hs" "src.hs" "dst.hs"
              readFile "dst.hs" `shouldReturn` unlines [
                  "{-# LINE 1 \"src.hs\" #-}"
                , "module Foo (String.length) where"
                , "import qualified String"
                , "{-# LINE 1 \"src.hs\" #-}"
                , ""
                ]

    context "when pre-processing identifiers" $ do
      it "desugars postfix bangs" $ do
        unlines [
            "foo! :: Int"
          , "foo! = 23"
          ] `shouldDesugarTo` unlines [
            "fooᴉ :: Int"
          , "fooᴉ = 23"
          ]

      it "desugars postfix question marks" $ do
          unlines [
              "foo? :: Int"
            , "foo? = 23"
            ] `shouldDesugarTo` unlines [
              "fooʔ :: Int"
            , "fooʔ = 23"
            ]

      it "does not desugar !-operators" $ do
        "foo = xs ! n" `shouldDesugarTo` "foo = xs ! n"

      context "when a postfix bang is followed by a dot" $ do
        it "desugars postfix bangs" $ do
          "foo = xs.tail!.head!" `shouldDesugarTo` "foo = xs.tailᴉ.headᴉ"

      context "with a qualified name" $ do
        it "desugars postfix bangs" $ do
          "foo = Bar.baz!" `shouldDesugarTo` "foo = Bar.bazᴉ"

      context "within a function argument" $ do
        it "desugars postfix bangs" $ do
          "foo(Bar.baz!)" `shouldDesugarTo` "({-# COLUMN 1 #-}foo(Bar.bazᴉ){-# COLUMN 13 #-})"

      context "within an import list" $ do
        it "desugars postfix bangs" $ do
          "import Foo (bar!)" `shouldDesugarTo` "import Foo (barᴉ)"

    context "when pre-processing use-statements" $ do
      context "with an unqualified module name" $ do
        it "desugars the use-statement" $ do
          "use Foo" `shouldDesugarTo` "import{-# COLUMN 4 #-} Foo qualified{-# COLUMN 8 #-}"

      context "with a qualified module name" $ do
        it "desugars the use-statement" $ do
          "use Foo.Bar" `shouldDesugarTo` "import{-# COLUMN 4 #-} Foo.Bar qualified as Bar{-# COLUMN 12 #-}"

      context "with `as` specified" $ do
        it "desugars the use-statement" $ do
          "use Foo.Bar as Baz" `shouldDesugarTo` "import{-# COLUMN 4 #-} Foo.Bar qualified{-# COLUMN 12 #-} as Baz"

    context "when pre-processing function calls" $ do
      it "desugars a function call with a single argument" $ do
        "foo(bar baz)" `shouldDesugarTo` "({-# COLUMN 1 #-}foo(bar baz){-# COLUMN 12 #-})"

      it "desugars a function call with multiple arguments" $ do
        "foo(bar, baz, 23)" `shouldDesugarTo` "({-# COLUMN 1 #-}foo(bar)({-# COLUMN 9 #-} baz)({-# COLUMN 14 #-} 23){-# COLUMN 17 #-})"

      context "with a qualified name" $ do
        it "desugars function calls" $ do
          "String.foo(bar)\n" `shouldDesugarTo` unlines [
              "import qualified String"
            , "{-# LINE 1 \"main.hs\" #-}"
            , "({-# COLUMN 1 #-}String.foo(bar){-# COLUMN 15 #-})"
            ]

    context "when pre-processing method chains" $ do
      it "desugars method chains" $ do
        "foo.bar(23).baz" `shouldDesugarTo` "({-# COLUMN 1 #-}foo.bar(23){-# COLUMN 11 #-}).baz"

      context "with a function call as the subject" $ do
        it "desugars method chains" $ do
          "foo(x).bar(y).baz(z)" `shouldDesugarTo` "(({-# COLUMN 1 #-}({-# COLUMN 1 #-}foo(x){-# COLUMN 6 #-}).bar(y){-# COLUMN 13 #-}).baz(z){-# COLUMN 20 #-})"

        context "with a qualified name" $ do
          it "desugars method chains" $ do
            "String.foo(x).bar(y).baz(z)\n" `shouldDesugarTo` unlines [
                "import qualified String"
              , "{-# LINE 1 \"main.hs\" #-}"
              , "(({-# COLUMN 1 #-}({-# COLUMN 1 #-}String.foo(x){-# COLUMN 13 #-}).bar(y){-# COLUMN 20 #-}).baz(z){-# COLUMN 27 #-})"
              ]

      context "with a string as the subject" $ do
        it "desugars method chains" $ do
          "\"foo\".bar(23).baz(42)" `shouldDesugarTo` "(({-# COLUMN 1 #-}\"foo\".bar(23){-# COLUMN 13 #-}).baz(42){-# COLUMN 21 #-})"

      context "with a list as the subject" $ do
        it "desugars method chains" $ do
          "[foo].bar(23).baz(42)" `shouldDesugarTo` "(({-# COLUMN 1 #-}[foo].bar(23){-# COLUMN 13 #-}).baz(42){-# COLUMN 21 #-})"

      context "with an expression in parentheses as the subject" $ do
        it "desugars method chains" $ do
          "(foo, bar).baz(23)" `shouldDesugarTo` "({-# COLUMN 1 #-}(foo, bar).baz(23){-# COLUMN 18 #-})"

    context "when pre-processing string literals" $ do
      it "desugars string interpolation" $ do
        "foo = \"foo {bar 23} baz\".toUpper" `interpolationShouldDesugarTo` mconcat [
            "foo = (\"foo \" <> Solid.ToString.toString ({-# COLUMN 13 #-}bar 23) <> \" baz\"{-# COLUMN 24 #-}).toUpper"
          ]

      it "desugars interpolation abstractions" $ do
        "foo = \"foo {} bar {} baz\"" `interpolationShouldDesugarTo` mconcat [
            "foo = (\\ _1 _2 -> \"foo \" <> Solid.ToString.toString ({-# COLUMN 13 #-}_1{-# COLUMN 13 #-}) <> \" bar \" <> Solid.ToString.toString ({-# COLUMN 20 #-}_2{-# COLUMN 20 #-}) <> \" baz\"{-# COLUMN 25 #-})"
          ]

      it "accepts string literals with multiple interpolations" $ do
        "foo = \"foo { 23 } bar { 42 } baz\"" `interpolationShouldDesugarTo` mconcat [
            "foo = (\"foo \" <> Solid.ToString.toString ({-# COLUMN 13 #-} 23 ) <> \" bar \" <> Solid.ToString.toString ({-# COLUMN 24 #-} 42 ) <> \" baz\"{-# COLUMN 33 #-})"
          ]

      it "accepts nested interpolations" $ do
        "foo = \"foo { \"x-{23}-x\" } baz\"" `interpolationShouldDesugarTo` mconcat [
            "foo = (\"foo \" <> Solid.ToString.toString ({-# COLUMN 13 #-} (\"x-\" <> Solid.ToString.toString ({-# COLUMN 18 #-}23) <> \"-x\"{-# COLUMN 23 #-}) ) <> \" baz\"{-# COLUMN 30 #-})"
          ]

      context "with an interpolated expression at the beginning of a string" $ do
        it "omits empty string segments" $ do
          "foo = \"{bar 23} baz\".toUpper" `interpolationShouldDesugarTo` mconcat [
              "foo = (Solid.ToString.toString ({-# COLUMN 9 #-}bar 23) <> \" baz\"{-# COLUMN 20 #-}).toUpper"
            ]

      context "with an interpolated expression at the end of a string" $ do
        it "omits empty string segments" $ do
          "foo = \"foo {bar 23}\".toUpper" `interpolationShouldDesugarTo` mconcat [
              "foo = (\"foo \" <> Solid.ToString.toString ({-# COLUMN 13 #-}bar 23){-# COLUMN 20 #-}).toUpper"
            ]

      context "when two interpolated expressions are next to each other" $ do
        it "omits empty string segments" $ do
          "foo = \"foo { 23 }{ 42 } baz\"" `interpolationShouldDesugarTo` mconcat [
              "foo = (\"foo \" <> Solid.ToString.toString ({-# COLUMN 13 #-} 23 ) <> Solid.ToString.toString ({-# COLUMN 19 #-} 42 ) <> \" baz\"{-# COLUMN 28 #-})"
            ]

      context "when an opening curly bracket is preceded by a backslash" $ do
        it "treats the opening curly bracket as a literal '{'" $ do
          "foo = \"foo \\{bar} baz\"" `shouldDesugarTo` "foo = \"foo {bar} baz\"{-# COLUMN 23 #-}"
