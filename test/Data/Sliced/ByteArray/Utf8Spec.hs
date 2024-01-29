{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Data.Sliced.ByteArray.Utf8Spec (spec, arbitrary) where

import Helper hiding (pack, Bytes, lines, unlines, words, unwords, take, drop)

import Data.Sliced.ByteArray.Unsafe

use Gen
use Range

import Data.Sliced.ByteArray.Conversion (unsafeToText, fromText)
import Data.Sliced.ByteArray.Utf8 as Utf8

import Data.Sliced.ByteArraySpec (bytesWith)
use Data.Text

string :: Gen [Char]
string = Gen.list (Range.linear 0 10) Gen.unicodeAny

char :: MonadGen m => m Char
char = Gen.choice [Gen.ascii, Gen.latin1, Gen.unicodeScalar]

utf8 :: MonadGen m => Range Int -> m ByteArray
utf8 = bytesWith pack char

arbitrary :: MonadGen m => m ByteArray
arbitrary = utf8 (Range.linear 0 10)

spec :: Spec
spec = do
  describe "pack" $ do
    it "behaves like Data.Text.pack" $ do
      input <- forAll string
      Utf8.pack input === fromText (Text.pack input)

  describe "unsafeUnpack" $ do
    it "behaves like Data.Text.unpack" $ do
      input <- forAll arbitrary
      Utf8.unsafeUnpack input === Text.unpack (unsafeToText input)

  describe "singleton" $ do
    it "behaves like Data.Text.singleton" $ do
      c <- forAll Gen.unicodeAny
      Utf8.singleton c === fromText (Text.singleton c)

  describe "empty" $ do
    it "behaves like Data.Text.empty" $ do
      Utf8.empty `shouldBe` fromText (Text.empty)

  describe "null" $ do
    it "behaves like Data.Text.null" $ do
      input <- forAll arbitrary
      Utf8.null input === Text.null (unsafeToText input)

  describe "length" $ do
    it "behaves like Data.Text.length" $ do
      input <- forAll arbitrary
      Utf8.length input === Text.length (unsafeToText input)

  describe "lines" $ do
    it "behaves like Data.Text.lines" $ do
      input <- unlines <$> forAll (Gen.list (Range.linear 0 10) arbitrary)
      lines input === map fromText (Text.lines (unsafeToText input))

  describe "words" $ do
    it "behaves like Data.Text.words" $ do
      input <- unwords <$> forAll (Gen.list (Range.linear 0 10) arbitrary)
      words input === map fromText (Text.words (unsafeToText input))

  describe "unlines" $ do
    it "behaves like Data.Text.unlines" $ do
      xs <- forAll $ Gen.list (Range.linear 0 10) arbitrary
      unlines xs === fromText (Text.unlines $ map unsafeToText xs)

  describe "unwords" $ do
    it "behaves like Data.Text.unwords" $ do
      xs <- forAll $ Gen.list (Range.linear 0 10) arbitrary
      unwords xs === fromText (Text.unwords $ map unsafeToText xs)

  describe "take" $ do
    context "with a non-negative number" $ do
      it "takes from the beginning of the list" $ do
        Utf8.take 3 "fλλbλrbλz" `shouldBe` "fλλ"

    context "with a negative number" $ do
      it "takes from the end of the list" $ do
        Utf8.take -3 "fλλbλrbλz" `shouldBe` "bλz"

  describe "drop" $ do
    context "with a non-negative number" $ do
      it "drops from the beginning of the list" $ do
        Utf8.drop 3 "fλλbλrbλz" `shouldBe` "bλrbλz"

    context "with a negative number" $ do
      it "drops from the end of the list" $ do
        Utf8.drop -3 "fλλbλrbλz" `shouldBe` "fλλbλr"

  describe "slice" $ do
    it "slices a byte array" $ do
      start <- forAll $ Gen.int (Range.constant 0 20)
      end <- forAll $ Gen.int (Range.constant 0 20)
      input <- forAll arbitrary
      slice start end input === drop start (take end input)

    context "with a negative start index" $ do
      it "slices a byte array" $ do
        start <- forAll $ Gen.int (Range.constant -20 -1)
        end <- forAll $ Gen.int (Range.constant 0 20)
        input <- forAll arbitrary
        slice start end input === drop (Utf8.length input + start).max(0) (take end input)

    context "with a negative end index" $ do
      it "slices a byte array" $ do
        start <- forAll $ Gen.int (Range.constant 0 20)
        end <- forAll $ Gen.int (Range.constant -20 -1)
        input <- forAll arbitrary
        slice start end input === drop start (drop end input)

    context "with a negative start index and a negative end index" $ do
      it "slices a byte array" $ do
        start <- forAll $ Gen.int (Range.constant -20 -1)
        end <- forAll $ Gen.int (Range.constant -20 -1)
        input <- forAll arbitrary
        slice start end input === drop end (take start input)

  describe "splitAt" $ do
    it "is reversed by (<>)" $ do
      input <- forAll arbitrary
      n <- forAll $ Gen.int (Range.constant -20 20)
      uncurry (<>) (Utf8.splitAt n input) === input

    context "with a non-negative number" $ do
      it "splits, counting from the beginning of the list" $ do
        Utf8.splitAt 3 "foobarbaz" === ("foo", "barbaz")
        input <- forAll arbitrary
        n <- forAll $ Gen.int (Range.constant 0 20)
        Utf8.splitAt n input === (Utf8.take n &&& Utf8.drop n) input

    context "with a negative number" $ do
      it "splits, counting from the end of the list" $ do
        Utf8.splitAt -3 "foobarbaz" === ("foobar", "baz")
        input <- forAll arbitrary
        n <- forAll $ Gen.int (Range.constant -20 -1)
        Utf8.splitAt n input === (Utf8.drop n &&& Utf8.take n) input

  describe "split" $ do
    context "with an empty separator" $ do
      it "splits into chunks of size one" $ do
        Utf8.split "" "fλλbλrbλz" `shouldBe` ["f", "λ", "λ", "b", "λ", "r", "b", "λ", "z"]

  describe "chunksOf" $ do
    it "splits a byte array into chunks of a specified size" $ do
      Utf8.chunksOf 2 "fλλbλrbλz" `shouldBe` ["fλ", "λb", "λr", "bλ", "z"]

    context "with an empty byte array" $ do
      it "returns the empty list" $ do
        Utf8.chunksOf 2 "" `shouldBe` []

    context "with a positive chunk size" $ do
      it "is reversed by mconcat" $ do
        input <- forAll arbitrary
        n <- forAll $ Gen.int (Range.constant 1 10)
        mconcat (Utf8.chunksOf n input) === input

    context "with a chunk size of 0" $ do
      it "returns the empty list" $ do
        Utf8.chunksOf 0 "fλλbλrbλz" `shouldBe` []

    context "with a negative chunk size" $ do
      it "returns the empty list" $ do
        Utf8.chunksOf -3 "fλλbλrbλz" `shouldBe` []
