{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.String.TypeSpec (spec) where

import qualified Prelude

import           Helper
import qualified Gen
import qualified Range

invalidUtf8 :: ByteString
invalidUtf8 = Bytes "foo \xc3\x28 bar"

listOfUpTo :: MonadGen m => Int -> m a -> m [a]
listOfUpTo n = Gen.list (Range.linear 0 n)

spec :: Spec
spec = do
  describe "Ord String" $ do
    it "behaves like Ord [Char]" $ do
      xs <- forAll $ listOfUpTo 10 Gen.unicodeScalar
      ys <- forAll $ listOfUpTo 10 Gen.unicodeScalar
      compare (pack xs) (pack ys) === compare xs ys

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
      xs <- forAll $ listOfUpTo 10 Gen.unicodeScalar
      unpack (pack xs) === xs

  describe "lines" $ do
    it "breaks a string into separate lines" $ do
      xs :: [Char] <- forAll $ listOfUpTo 100 $ Gen.frequency [
          (1, pure '\n')
        , (1, pure '\r')
        , (5, Gen.unicodeScalar)
        ]
      map unpack xs.pack.lines === Prelude.lines xs

  describe "unlines" $ do
    it "joins lines, appending a terminating newline after each" $ do
      xs :: [String] <- forAll $ listOfUpTo 10 (pack <$> listOfUpTo 10 Gen.unicodeAny)
      xs.unlines === pack (Prelude.unlines (map unpack xs))

  describe ".length" $ do
    it "returns the length of a String" $ do
      xs <- forAll $ listOfUpTo 10 Gen.unicodeAny
      xs.pack.length === length xs
