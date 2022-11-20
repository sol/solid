{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.StringSpec (spec) where

import           Prelude()
import           Helper

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

instance HasField "toText" String Text where
  getField = T.decodeUtf8 . unBytes

invalidUtf8 :: ByteString
invalidUtf8 = Bytes "foo \xc3\x28 bar"

spec :: Spec
spec = do
  describe "Ord String" $ do
    it "behaves like Ord [Char]" $ do
      property $ \ xs ys -> do
        compare (pack xs) (pack ys) `shouldBe` compare xs ys

  describe ".asString" $ do
    it "converts a ByteString to a String" $ do
      let
        input :: ByteString
        input = Bytes "foo"
      input.asString `shouldBe` Just "foo"

    context "on invalid UTF-8" $ do
      it "returns Nothing" $ do
        invalidUtf8.asString `shouldBe` Nothing

  describe ".asString!" $ do
    it "converts a ByteString to a String" $ do
      let
        input :: ByteString
        input = Bytes "foo"
      input.asString! `shouldBe` "foo"

    context "on invalid UTF-8" $ do
      it "throws an exception" $ do
        evaluate invalidUtf8.asString! `shouldThrow` UnicodeDecodeError

  describe "pack" $ do
    it "creates a String from a list of Char" $ do
      pack ("foo" :: [Char]) `shouldBe` ("foo" :: String)

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

  describe ".length" $ do
    it "returns the length of a String" $ do
      property $ \ xs -> do
        (pack xs).length `shouldBe` T.length (T.pack xs)
