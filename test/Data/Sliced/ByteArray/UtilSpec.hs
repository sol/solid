module Data.Sliced.ByteArray.UtilSpec (spec) where

import HaskellPrelude
import Control.Exception (evaluate)
import Test.Hspec

import Data.Sliced.ByteArray.Util

spec :: Spec
spec = do
  describe "checkedAdd" $ do
    it "throws an exception on overflow" $ do
      evaluate (checkedAdd "foo" maxBound 1) `shouldThrow` errorCall "Data.Sliced.ByteArray.foo: size overflow"

  describe "checkedSum" $ do
    it "throws an exception on overflow" $ do
      evaluate (checkedSum "foo" [maxBound - 5, 3, 2, 1]) `shouldThrow` errorCall "Data.Sliced.ByteArray.foo: size overflow"

  describe "checkedMultiply" $ do
    it "throws an exception on overflow" $ do
      let
        n :: Int
        n = ceiling . sqrt @Double $ fromIntegral (maxBound :: Int)
      evaluate (checkedMultiply "foo" n n) `shouldThrow` errorCall "Data.Sliced.ByteArray.foo: size overflow"
