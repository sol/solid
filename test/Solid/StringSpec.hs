{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.StringSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Prelude()
import           Solid

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

instance HasField "toText" String Text where
  getField = T.decodeUtf8 . unBytes

spec :: Spec
spec = do
  describe "unpack" $ do
    it "is inverse to pack" $ do
      property $ \ xs -> do
        unpack (pack xs) `shouldBe` xs

  describe "lines" $ do
    it "breaks a string into separate lines" $ do
      property $ \ xs -> do
        map (.toText) (pack xs).lines `shouldBe` T.lines (T.pack xs)

  describe "unlines" $ do
    it "joins lines, appending a terminating newline after each" $ do
      property $ \ xs -> do
        (map pack xs).unlines.toText `shouldBe` T.unlines (map T.pack xs)

  describe "String.length" $ do
    it "returns the length of a String" $ do
      property $ \ xs -> do
        (pack xs).length `shouldBe` T.length (T.pack xs)
