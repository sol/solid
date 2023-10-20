{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Data.Sliced.ByteArray.Utf8Spec (spec) where

import Helper hiding (pack, Bytes, lines, unlines, words, unwords)

import Data.Sliced.ByteArray.Unsafe

use Gen
use Range
use Hedgehog.Internal.Gen as Org

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

    xit "foo" $ do
      -- input :: [Char] <- forAll $ Gen.list (Range.constant 0 (1024 * 10)) Gen.unicodeAny
      -- input :: [Char] <- forAll $ Gen.list (Range.constant 0 (1024 * 10)) (Gen.enum minBound maxBound)
      -- input :: [Char] <- forAll $ Gen.list (Range.singleton (1024 * 10)) (Gen.enum'fast minBound maxBound)
      let n = 256 * 2
      input :: [Char] <- forAll $ Gen.list (Range.singleton n) (Org.fastEnumShrink minBound maxBound)
      List.length input === 1

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
