{-# LANGUAGE OverloadedStrings #-}
module Solid.PP.EditSpec (spec) where

import           Test.Hspec

import           Solid.PP.BuilderSpec ()

import           Solid.PP.Edit hiding (edit)

spec :: Spec
spec = do
  describe "columnPragma" $ do
    context "when original is shorter than replacement" $ do
      it "generates a COLUMN pragma" $ do
        let edit = Replace (Just 10) 20 2 "foo"
        columnPragma edit `shouldBe` Just "{-# COLUMN 12 #-}"

    context "when original is longer than replacement" $ do
      it "generates a COLUMN pragma" $ do
        let edit = Replace (Just 10) 20 5 "foo"
        columnPragma edit `shouldBe` Just "{-# COLUMN 15 #-}"

    context "when original and replacement have the same length" $ do
      it "does not generate a COLUMN pragma" $ do
        let edit = Replace (Just 10) undefined 3 "foo"
        columnPragma edit `shouldBe` Nothing

    context "without a StartColumn" $ do
      it "does not generate a COLUMN pragma" $ do
        let edit = Replace Nothing undefined undefined undefined
        columnPragma edit `shouldBe` Nothing
