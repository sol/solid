{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.PP.BuilderSpec (spec) where

import           Prelude ()
import           Solid.PP.IO

import           Test.Hspec

import           Solid.PP.Builder (Builder)
import qualified Solid.PP.Builder as Builder

instance Show Builder where
  showsPrec p = showsPrec p . Builder.toText

instance Eq Builder where
  a == b = Builder.toText a == Builder.toText b

spec :: Spec
spec = do
  describe "unlines" $ do
    it "appends newlines, then concatenates" $ do
      Builder.unlines ["foo", "bar", "baz"] `shouldBe` "foo\nbar\nbaz\n"

    context "with the empty list" $ do
      it "returns the empty string" $ do
        Builder.unlines [] `shouldBe` ""
