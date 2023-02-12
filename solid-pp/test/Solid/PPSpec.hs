{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Solid.PPSpec (spec) where

import           Prelude ()
import           Solid.PP.IO

import           Data.Bifunctor

import           Test.Hspec
import           Test.Mockery.Directory

import           Solid.PP

infix 1 `shouldDesugarTo`

shouldDesugarTo :: HasCallStack => Text -> Text -> Expectation
shouldDesugarTo input expected = do
  let file = "main.hs"
  writeFile file input
  run file file file
  readFile file `shouldReturn` ("{-# LINE 1 " <> pack (show file) <> " #-}\n" <> expected)

infix 1 `shouldFailWith`

shouldFailWith :: HasCallStack => IO () -> String -> Expectation
shouldFailWith action expected = do
  (first show -> r) :: Either SomeException () <- try action
  case r of
    Left err -> err `shouldBe` expected
    Right () -> r `shouldBe` Left expected

spec :: Spec
spec = do
  describe "run" $ around_ inTempDirectory $ do
    context "on error" $ do
      it "reports error locations" $ do
        writeFile "cur.hs" $ unlines [
            "foo :: Int"
          , "  {-"
          , "foo = 23"
          ]
        run "src.hs" "cur.hs" "dst.hs" `shouldFailWith` "src.hs:2:3: error: unterminated `{-' at end of input"

      it "takes LINE pragmas into account" $ do
        writeFile "cur.hs" $ unlines [
            "{-# LINE 23 \"foo.hs\" #-}"
          , "foo :: Int"
          , "{-"
          , "foo = 23"
          ]
        run "src.hs" "cur.hs" "dst.hs" `shouldFailWith` "foo.hs:24:1: error: unterminated `{-' at end of input"

      it "does not reports failures for GHC2021 syntax" $ do
        writeFile "cur.hs" $ unlines [
            "foo :: Int"
          , "foo = 23_0"
          , "{-"
          ]
        run "src.hs" "cur.hs" "dst.hs" `shouldFailWith` "src.hs:3:1: error: unterminated `{-' at end of input"

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
        "foo = \"foo {bar} baz\".toUpper" `shouldDesugarTo` mconcat [
            "foo = (\"foo \" <> toString ({-# COLUMN 13 #-}bar) <> \" baz\"){-# COLUMN 22 #-}.toUpper"
          ]

      it "accepts string literals with multiple interpolations" $ do
        "foo = \"foo { 23 } bar { 42 } baz\"" `shouldDesugarTo` mconcat [
            "foo = (\"foo \" <> toString ({-# COLUMN 13 #-} 23 ) <> \" bar \" <> toString ({-# COLUMN 24 #-} 42 ) <> \" baz\"){-# COLUMN 34 #-}"
          ]

      it "accepts nested interpolations" $ do
        "foo = \"foo { \"x-{23}-x\" } baz\"" `shouldDesugarTo` mconcat [
            "foo = (\"foo \" <> toString ({-# COLUMN 13 #-} (\"x-\" <> toString ({-# COLUMN 18 #-}23) <> \"-x\"){-# COLUMN 24 #-} ) <> \" baz\"){-# COLUMN 31 #-}"
          ]

      context "when an opening curly bracket is preceded by a backslash" $ do
        it "treats the opening curly bracket as a literal '{'" $ do
          "foo = \"foo \\{bar} baz\"" `shouldDesugarTo` "foo = \"foo {bar} baz\"{-# COLUMN 23 #-}"
