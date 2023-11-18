{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE OverloadedLists #-}
module Data.Sliced.ByteArray.UnsafeSpec (spec) where

import Helper hiding (empty)
import Data.Sliced.ByteArraySpec (arbitrary)

import Data.Primitive.ByteArray (isByteArrayPinned, byteArrayContents)

import Data.Sliced.ByteArray.Unsafe

spec :: Spec
spec = do
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
