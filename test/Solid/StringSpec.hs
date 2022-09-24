module Solid.StringSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Prelude()
import           Solid

import qualified Data.Text as T

spec :: Spec
spec = do
  describe "unpack" $ do
    it "is inverse to pack" $ do
      property $ \ xs -> do
        unpack (pack xs) `shouldBe` xs

  describe "String.length" $ do
    it "returns the length of a String" $ do
      property $ \ xs -> do
        (pack xs).length `shouldBe` T.length (T.pack xs)
