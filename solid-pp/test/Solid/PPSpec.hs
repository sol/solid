module Solid.PPSpec (spec) where

import           Test.Hspec

import           Solid.PP

spec :: Spec
spec = do
  describe "pp" $ do
    context "when pre-processing string literals" $ do
      it "desugars string interpolation" $ do
        pp "foo = \"foo {bar} baz\"" `shouldBe` "foo = (\"foo \" <> toString (bar) <> \" baz\")"

      context "when an opening curly bracket is preceded by a backslash" $ do
        it "treats the opening curly bracket as a literal '{'" $ do
          pp "foo = \"foo \\{bar} baz\"" `shouldBe` "foo = \"foo {bar} baz\""
