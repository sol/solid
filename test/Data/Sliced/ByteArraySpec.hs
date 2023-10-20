{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Sliced.ByteArraySpec (arbitrary, spec) where

import Helper
use Gen
use Range

import Data.Semigroup
import GHC.Exts (fromList)

import Data.Sliced.ByteArray as ByteArray
import Data.Sliced.ByteArray.Unsafe as ByteArray

import Hedgehog.Classes
import Hedgehog.Internal.Property

word8 :: MonadGen m => m Word8
word8 = Gen.word8 Range.constantBounded

bytes :: MonadGen m => Range Int -> m ByteArray
bytes range = do
  size <- Gen.int range
  off <- Gen.int (Range.constant 0 size)
  end <- Gen.int (Range.constant off size)
  let len = end - off
  arr <- fromList <$> Gen.list (Range.singleton size) word8
  return ByteArray{..}

arbitrary :: MonadGen m => m ByteArray
arbitrary = bytes (Range.linear 0 10)

satisfies :: HasCallStack => Gen a -> (Gen a -> Laws) -> Spec
satisfies gen laws = do
  describe (className <> " instance") $ do
    forM_ properties $ \ (name, p) -> do
      it ("satisfies " <> name) (propertyTest p)
  where
    Laws className properties = laws gen

spec :: Spec
spec = do
  arbitrary `satisfies` eqLaws
  arbitrary `satisfies` showLaws
  arbitrary `satisfies` semigroupLaws
  arbitrary `satisfies` monoidLaws

  describe "show" $ do
    context "with valid UTF-8" $ do
      it "shows a textual representation" $ do
        let input = "foo" :: ByteArray
        show input `shouldBe` "\"foo\""

    context "with invalid UTF-8" $ do
      it "shows a list of bytes" $ do
        let input = "fo" <> ByteArray.pack [0xf6]
        show input `shouldBe` "[0x66, 0x6f, 0xf6]"

    it "correctly handles length and offset" $ do
      input <- forAll arbitrary
      show input === show (ByteArray.copy input)

  describe "pack" $ do
    it "packs a list of bytes into a ByteArray" $ do
      ByteArray.pack [102, 111, 111, 98, 97, 114] `shouldBe` "foobar"

    it "is inverse to unpack" $ do
      input <- forAll arbitrary
      ByteArray.pack (ByteArray.unpack input) === input

  describe "unpack" $ do
    it "unpacks a list of bytes from a ByteArray" $ do
      ByteArray.unpack "foobar" `shouldBe` [102, 111, 111, 98, 97, 114]

  describe "append" $ do
    it "appends two byte arrays" $ do
      append "foo" "bar" `shouldBe` "foobar"

  describe "concat" $ do
    it "concatenates a list of byte arrays" $ do
      ByteArray.concat ["foo", "bar", "baz"] `shouldBe` "foobarbaz"

  describe "isValidUtf8" $ do
    context "with valid UTF-8" $ do
      it "returns True" $ do
        input <- forAll $ Gen.list (Range.constant 0 10) Gen.unicodeScalar
        isValidUtf8 (fromString input) === True

    context "with invalid UTF-8" $ do
      it "returns False" $ do
        let input = ByteArray [0, 128] 1 1
        isValidUtf8 input `shouldBe` False

  describe "stimes" $ do
    it "is defined on 0" $ do
      stimes @ByteArray (0 :: Integer) "foo" `shouldBe` ""
