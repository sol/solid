{-# LANGUAGE OverloadedStrings #-}
module Solid.PPSpec (spec) where

import           Prelude ()
import           Solid.PP.IO

import           Test.Hspec
import           Test.HUnit (assertFailure)
import           Test.Hspec.Expectations.Contrib (annotate)
import           Test.Mockery.Directory
import           Data.Set (Set)
import qualified Data.Set as Set

import           Solid.PP.Parser

import           Solid.PP hiding (parseModuleHeader)
import qualified Solid.PP as PP

infix 1 `shouldDesugarTo`

shouldDesugarTo :: HasCallStack => Text -> Text -> Expectation
shouldDesugarTo input expected = do
  let file = "main.hs"
  writeFile file input
  Success <- run file file file
  readFile file `shouldReturn`
       "{-# LINE 1 " <> pack (show file) <> " #-}\n"
    <> expected

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

parseModuleHeader :: HasCallStack => Text -> (ModuleHeader -> IO a) -> IO a
parseModuleHeader input action = case parse extensions "src.hs" input of
  Left err -> assertFailure err
  Right nodes -> annotate (show [t | Token _ t <- nodes]) $ do
    action (PP.parseModuleHeader nodes)

spec :: Spec
spec = do
  describe "usedModules" $ do
    let
      modules :: HasCallStack => Text -> Set Module
      modules = usedModules . either undefined id . parse extensions "src.hs"

    context "with a qualified identifier" $ do
      it "extracts module name" $ do
        modules "foo = String.length" `shouldBe` Set.fromList ["String"]

    context "within an interpolated string" $ do
      it "extracts module names" $ do
        modules "foo \"some {Foo.x} test {Bar.x} input\"" `shouldBe` Set.fromList ["Solid.ToString", "Bar", "Foo"]

  describe "parseModuleHeader" $ do
    it "parses the module header" $ do
      parseModuleHeader "module Foo where" (`shouldBe` ModuleHeader (Just "Foo") 16 "src.hs" 1)

    it "ignores comments" $ do
      parseModuleHeader "module {- some comment -} Foo where" (`shouldBe` ModuleHeader (Just "Foo") 35 "src.hs" 1)

    context "with a hierarchical module" $ do
      it "parses the module header" $ do
        parseModuleHeader "module Foo.Bar where" (`shouldBe` ModuleHeader (Just "Foo.Bar") 20 "src.hs" 1)

  describe "run" $ around_ inTempDirectory $ do
    context "on error" $ do
      it "reports error locations" $ do
        writeFile "cur.hs" $ unlines [
            "foo :: Int"
          , "  {-"
          , "foo = 23"
          ]
        run "src.hs" "cur.hs" "dst.hs" `shouldReturn` Failure "src.hs:2:3: error: [GHC-21231] unterminated `{-' at end of input"

      it "takes LINE pragmas into account" $ do
        writeFile "cur.hs" $ unlines [
            "{-# LINE 23 \"foo.hs\" #-}"
          , "foo :: Int"
          , "{-"
          , "foo = 23"
          ]
        run "src.hs" "cur.hs" "dst.hs" `shouldReturn` Failure "foo.hs:24:1: error: [GHC-21231] unterminated `{-' at end of input"

      it "does not report failures for GHC2021 syntax" $ do
        writeFile "cur.hs" $ unlines [
            "foo :: Int"
          , "foo = 23_0"
          , "{-"
          ]
        run "src.hs" "cur.hs" "dst.hs" `shouldReturn` Failure "src.hs:3:1: error: [GHC-21231] unterminated `{-' at end of input"

    context "when pre-processing imports" $ do
      it "implicitly imports well-know modules" $ do
        writeFile "cur.hs" $ unlines [
            "foo :: String -> Int"
          , "foo = String.length"
          ]
        run "src.hs" "cur.hs" "dst.hs" `shouldReturn` Success
        readFile "dst.hs" `shouldReturn` unlines [
            "{-# LINE 1 \"src.hs\" #-}"
          , "import qualified String"
          , "{-# LINE 1 \"src.hs\" #-}"
          , "foo :: String -> Int"
          , "foo = String.length"
          ]

      context "when the same module name occurs in multiple qualified identifiers" $ do
        it "adds a single import" $ do
          writeFile "cur.hs" $ unlines [
              "foo :: String -> Int"
            , "foo = String.length"
            , "bar :: String -> Int"
            , "bar = String.length"
            ]
          run "src.hs" "cur.hs" "dst.hs" `shouldReturn` Success
          readFile "dst.hs" `shouldReturn` unlines [
              "{-# LINE 1 \"src.hs\" #-}"
            , "import qualified String"
            , "{-# LINE 1 \"src.hs\" #-}"
            , "foo :: String -> Int"
            , "foo = String.length"
            , "bar :: String -> Int"
            , "bar = String.length"
            ]

      context "with LANGUAGE pragmas" $ do
        it "places implicit imports after the last pragma" $ do
          writeFile "cur.hs" $ unlines [
              "-- some comment"
            , "{-# LANGUAGE OverloadedStrings #-}"
            , "{-# OPTIONS_GHC -fno-warn-orphans #-}"
            , "foo :: String -> Int"
            , "{-# INLINE foo #-}"
            , "foo = String.length"
            ]
          run "src.hs" "cur.hs" "dst.hs" `shouldReturn` Success
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
            writeFile "cur.hs" $ unlines [
                "-- some comment"
              , "{-# LANGUAGE OverloadedStrings #-}"
              , "{-# OPTIONS_GHC -fno-warn-orphans #-}"
              ]
            run "src.hs" "cur.hs" "dst.hs" `shouldReturn` Success
            readFile "dst.hs" `shouldReturn` unlines [
                "{-# LINE 1 \"src.hs\" #-}"
              , "-- some comment"
              , "{-# LANGUAGE OverloadedStrings #-}"
              , "{-# OPTIONS_GHC -fno-warn-orphans #-}"
              ]

      context "with a module header" $ do
        it "places implicit imports after the module header" $ do
          writeFile "cur.hs" $ unlines [
              "{-# LANGUAGE OverloadedStrings #-}"
            , "module Foo where"
            , "foo :: String -> Int"
            , "foo = String.length"
            ]
          run "src.hs" "cur.hs" "dst.hs" `shouldReturn` Success
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
          writeFile "cur.hs" $ unlines [
              "module String where"
            , "foo = String.length"
            ]
          run "src.hs" "cur.hs" "dst.hs" `shouldReturn` Success
          readFile "dst.hs" `shouldReturn` unlines [
              "{-# LINE 1 \"src.hs\" #-}"
            , "module String where"
            , "foo = String.length"
            ]

        context "without any qualified names" $ do
          it "does not modify anything" $ do
            writeFile "cur.hs" $ unlines [
                "module Foo where"
              , "foo :: Int"
              , "foo = 23"
              ]
            run "src.hs" "cur.hs" "dst.hs" `shouldReturn` Success
            readFile "dst.hs" `shouldReturn` unlines [
                "{-# LINE 1 \"src.hs\" #-}"
              , "module Foo where"
              , "foo :: Int"
              , "foo = 23"
              ]

        context "with an export list" $ do
          it "places implicit imports after the module header" $ do
            writeFile "cur.hs" $ unlines [
                "module Foo (foo!) where"
              , "foo! :: String -> Int"
              , "foo! = String.length"
              ]
            run "src.hs" "cur.hs" "dst.hs" `shouldReturn` Success
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
              writeFile "cur.hs" $ unlines [
                  "module Foo (String.length) where"
                ]
              run "src.hs" "cur.hs" "dst.hs" `shouldReturn` Success
              readFile "dst.hs" `shouldReturn` unlines [
                  "{-# LINE 1 \"src.hs\" #-}"
                , "module Foo (String.length) where"
                , "import qualified String"
                , "{-# LINE 1 \"src.hs\" #-}"
                , ""
                ]

      context "with a partial module header" $ do
        it "does not implicitly import anything" $ do
          writeFile "cur.hs" $ unlines [
              "module Foo (foo)"
            , "foo :: String -> Int"
            , "foo = String.length"
            ]
          run "src.hs" "cur.hs" "dst.hs" `shouldReturn` Success
          readFile "dst.hs" `shouldReturn` unlines [
              "{-# LINE 1 \"src.hs\" #-}"
            , "module Foo (foo)"
            , "foo :: String -> Int"
            , "foo = String.length"
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

    context "when pre-processing string literals" $ do
      it "desugars string interpolation" $ do
        "foo = \"foo {bar 23} baz\".toUpper" `interpolationShouldDesugarTo` mconcat [
            "foo = (\"foo \" <> Solid.ToString.toString ({-# COLUMN 13 #-}bar 23) <> \" baz\").toUpper"
          ]

      it "desugars interpolation abstractions" $ do
        "foo = \"foo {} bar {} baz\"" `interpolationShouldDesugarTo` mconcat [
            "foo = (\\ _1 _2 -> \"foo \" <> Solid.ToString.toString ({-# COLUMN 13 #-}_1{-# COLUMN 13 #-}) <> \" bar \" <> Solid.ToString.toString ({-# COLUMN 20 #-}_2{-# COLUMN 20 #-}) <> \" baz\"){-# COLUMN 26 #-}"
          ]

      it "accepts string literals with multiple interpolations" $ do
        "foo = \"foo { 23 } bar { 42 } baz\"" `interpolationShouldDesugarTo` mconcat [
            "foo = (\"foo \" <> Solid.ToString.toString ({-# COLUMN 13 #-} 23 ) <> \" bar \" <> Solid.ToString.toString ({-# COLUMN 24 #-} 42 ) <> \" baz\"){-# COLUMN 34 #-}"
          ]

      it "accepts nested interpolations" $ do
        "foo = \"foo { \"x-{23}-x\" } baz\"" `interpolationShouldDesugarTo` mconcat [
            "foo = (\"foo \" <> Solid.ToString.toString ({-# COLUMN 13 #-} (\"x-\" <> Solid.ToString.toString ({-# COLUMN 18 #-}23) <> \"-x\"){-# COLUMN 24 #-} ) <> \" baz\"){-# COLUMN 31 #-}"
          ]

      context "with an interpolated expression at the beginning of a string" $ do
        it "omits empty string segments" $ do
          "foo = \"{bar 23} baz\".toUpper" `interpolationShouldDesugarTo` mconcat [
              "foo = (Solid.ToString.toString ({-# COLUMN 9 #-}bar 23) <> \" baz\").toUpper"
            ]

      context "with an interpolated expression at the end of a string" $ do
        it "omits empty string segments" $ do
          "foo = \"foo {bar 23}\".toUpper" `interpolationShouldDesugarTo` mconcat [
              "foo = (\"foo \" <> Solid.ToString.toString ({-# COLUMN 13 #-}bar 23)).toUpper"
            ]

      context "when two interpolated expressions are next to each other" $ do
        it "omits empty string segments" $ do
          "foo = \"foo { 23 }{ 42 } baz\"" `interpolationShouldDesugarTo` mconcat [
              "foo = (\"foo \" <> Solid.ToString.toString ({-# COLUMN 13 #-} 23 ) <> Solid.ToString.toString ({-# COLUMN 19 #-} 42 ) <> \" baz\"){-# COLUMN 29 #-}"
            ]

      context "when an opening curly bracket is preceded by a backslash" $ do
        it "treats the opening curly bracket as a literal '{'" $ do
          "foo = \"foo \\{bar} baz\"" `shouldDesugarTo` "foo = \"foo {bar} baz\"{-# COLUMN 23 #-}"
