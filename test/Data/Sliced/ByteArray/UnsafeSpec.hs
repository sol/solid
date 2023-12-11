{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE OverloadedLists #-}
module Data.Sliced.ByteArray.UnsafeSpec (spec) where

import Helper hiding (empty)
import Data.Sliced.ByteArraySpec (arbitrary)

import Data.Primitive.ByteArray (isByteArrayPinned, byteArrayContents)

import Data.Sliced.ByteArray.Unsafe
import Data.Sliced.ByteArray.Conversion
use Data.Sliced.ByteArray.Utf8Spec as Utf8

spec :: Spec
spec = do
  describe "compare" $ do
    it "behaves like compare for ByteString" $ do
      a <- forAll arbitrary
      b <- forAll arbitrary
      compare a b === compare (toByteString a) (toByteString b)

    it "behaves like compare for Text" $ do
      a <- forAll Utf8.arbitrary
      b <- forAll Utf8.arbitrary
      compare a b === compare (unsafeToText a) (unsafeToText b)

  describe "pin" $ do
    it "pins the underlying data" $ do
      input <- forAll arbitrary
      let pinned = pin input
      isByteArrayPinned pinned.arr === True
      pinned === input

  describe "copy" $ do
    it "copies the underlying data" $ do
      input <- forAll arbitrary
      let copied = copy input
      byteArrayContents copied.arr /== byteArrayContents input.arr
      copied === input

  describe "compact" $ do
    it "ensures that the underlying data is compact" $ do
      input <- forAll arbitrary
      let compacted = compact input
      compacted.off === 0
      compacted === input
