{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE OverloadedLists #-}
module Data.Sliced.ByteArray.ConversionSpec (spec) where

import Helper
import Data.Sliced.ByteArraySpec (arbitrary)
use Gen
use Range

use Data.ByteString.Lazy

import System.OsPath (unsafeFromChar)
use System.OsPath

import Data.Sliced.ByteArray.Conversion

spec :: Spec
spec = do
  describe "toByteString" $ do
    it "converts a ByteArray to a ByteString" $ do
      toByteString "foo" `shouldBe` "foo"

  describe "fromByteString" $ do
    it "is inverse to toByteString" $ do
      input <- forAll arbitrary
      fromByteString (toByteString input) === input

  describe "toShortByteString" $ do
    it "converts a ByteArray to a ShortByteString" $ do
      toShortByteString "foo" `shouldBe` "foo"

  describe "fromShortByteString" $ do
    it "is inverse to toShortByteString" $ do
      input <- forAll arbitrary
      fromShortByteString (toShortByteString input) === input

  describe "toLazyByteString" $ do
    it "converts a ByteArray to a LazyByteString" $ do
      toLazyByteString "foo" `shouldBe` "foo"

  describe "fromLazyByteString" $ do
    it "is inverse to toLazyByteString" $ do
      input <- forAll arbitrary
      fromLazyByteString (toLazyByteString input) === input

    it "correctly handles multiple chunks" $ do
      chunks <- forAll $ Gen.list (Range.constant 0 10) arbitrary
      let input = Lazy.fromChunks (map toByteString chunks)
      fromLazyByteString input === mconcat chunks

  describe "unsafeToText" $ do
    it "converts a ByteArray to a Text" $ do
      unsafeToText "foo" `shouldBe` "foo"

  describe "fromText" $ do
    it "is inverse to unsafeToText" $ do
      input <- forAll arbitrary
      fromText (unsafeToText input) === input

  describe "toOsPath" $ do
    it "converts a ByteArray to an OsPath" $ do
      toOsPath "foo" `shouldBe` OsPath.pack (map unsafeFromChar "foo")

  describe "fromOsPath" $ do
    it "is inverse to toOsPath" $ do
      input <- forAll arbitrary
      fromOsPath (toOsPath input) === input

  describe "toByteSlice" $ do
    it "converts a ByteArray to a ByteSlice" $ do
      toByteSlice "foo" `shouldBe` [102, 111, 111]

  describe "fromByteSlice" $ do
    it "is inverse to toByteSlice" $ do
      input <- forAll arbitrary
      fromByteSlice (toByteSlice input) === input
