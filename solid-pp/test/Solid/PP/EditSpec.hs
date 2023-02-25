{-# LANGUAGE OverloadedStrings #-}
module Solid.PP.EditSpec (spec) where

import           Test.Hspec

import           Solid.PP.Edit hiding (edit)

spec :: Spec
spec = do
  describe "columnPragma" $ do
    context "when original is shorter than replacement" $ do
      it "generates a COLUMN pragma" $ do
        let edit = Replace (Just 10) 20 2 "foo"
        columnPragma "" edit `shouldBe` Just 12

    context "when original is longer than replacement" $ do
      it "generates a COLUMN pragma" $ do
        let edit = Replace (Just 10) 20 5 "foo"
        columnPragma "" edit `shouldBe` Just 15

    context "when original and replacement have the same length" $ do
      it "does not generate a COLUMN pragma" $ do
        let edit = Replace (Just 10) undefined 3 "foo"
        columnPragma "" edit `shouldBe` Nothing

    context "without a StartColumn" $ do
      it "does not generate a COLUMN pragma" $ do
        let edit = Replace Nothing undefined undefined undefined
        columnPragma "" edit `shouldBe` Nothing

    context "workaround for GHC #23040" $ do
      -- https://gitlab.haskell.org/ghc/ghc/-/issues/23040
      let edit = Replace (Just 3) 3 3 "23"
      context "when original is not followed by a dot" $ do
        it "generates a COLUMN pragma" $ do
          let input = "foobarbaz"
          columnPragma input edit `shouldBe` Just 6

      context "when original is followed by a dot" $ do
        it "does not generate a COLUMN pragma" $ do
          let input = "foobar.baz"
          columnPragma input edit `shouldBe` Nothing
