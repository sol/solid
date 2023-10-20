{-# LANGUAGE CPP #-}
module Data.Sliced.ByteArray.UtilSpec (spec) where

#ifdef x86_64_HOST_ARCH
#define MAX_INT_IS_HUGE
#elif i386_HOST_ARCH
#else
#error untested ARCH
#endif

import HaskellPrelude
import Test.Hspec

import Data.Sliced.ByteArray.Util

spec :: Spec
spec = do
#ifdef MAX_INT_IS_HUGE
  describe "maxBound :: Int" $ do
    it "is huge" $ do
      toInteger (maxBound :: Int) `shouldBe` 9_223_372_036_854_775_807

  describe "checkedAdd" $ do
    it "does not throws an exception on overflow" $ do
      checkedAdd maxBound 1 `shouldBe` minBound

  describe "checkedSum" $ do
    it "does not throws an exception on overflow" $ do
      checkedSum [maxBound - 5, 3, 2, 1]`shouldBe` minBound
#else
  describe "maxBound :: Int" $ do
    it "is smallish" $ do
      toInteger (maxBound :: Int) `shouldBe` 2_147_483_647

  describe "checkedAdd" $ do
    it "throws an exception on overflow" $ do
      evaluate (checkedAdd maxBound 1) `shouldThrow` errorCall "size overflow"

  describe "checkedSum" $ do
    it "throws an exception on overflow" $ do
      evaluate (checkedSum [maxBound - 5, 3, 2, 1]) `shouldThrow` errorCall "size overflow"
#endif
