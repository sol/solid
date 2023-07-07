module Solid.Foreign.CSpec (spec) where

import Helper
import Gen qualified

import Solid.Foreign.C qualified as C

unicodeMin :: C.WChar
unicodeMin = C.toWChar minBound

unicodeMax :: C.WChar
unicodeMax = C.toWChar maxBound

spec :: Spec
spec = do
  describe "WChar" $ do
    describe "fromWChar" $ do
      it "converts a WChar to a Char" $ do
        C.fromWChar 65 `shouldBe` 'A'

      it "is inverse to toWChar" $ do
        c <- forAll Gen.unicodeAny
        (C.fromWChar . C.toWChar) c === c

      context "with the smallest Unicode code point" $ do
        it "converts a WChar to a Char" $ do
          C.fromWChar unicodeMin `shouldBe` minBound

      context "with an out-of-range value that is too small" $ do
        it "returns the Unicode replacement character" $ do
          C.fromWChar (pred unicodeMin) `shouldBe` '\xFFFD'

      context "with the largest Unicode code point" $ do
        it "converts a WChar to a Char" $ do
          C.fromWChar unicodeMax `shouldBe` maxBound

      context "with an out-of-range value that is too large" $ do
        it "returns the Unicode replacement character" $ do
          C.fromWChar (succ unicodeMax) `shouldBe` '\xFFFD'
