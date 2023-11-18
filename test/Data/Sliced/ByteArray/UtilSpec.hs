module Data.Sliced.ByteArray.UtilSpec (spec) where

import HaskellPrelude
import Control.Exception (evaluate)
import Test.Hspec

import Data.Sliced.ByteArray.Util

spec :: Spec
spec = do
  describe "checkedAdd" $ do
    it "throws an exception on overflow" $ do
      evaluate (checkedAdd maxBound 1) `shouldThrow` errorCall "size overflow"

  describe "checkedSum" $ do
    it "throws an exception on overflow" $ do
      evaluate (checkedSum [maxBound - 5, 3, 2, 1]) `shouldThrow` errorCall "size overflow"

  describe "checkedMultiply" $ do
    it "throws an exception on overflow" $ do
      let
        n :: Int
        n = ceiling . sqrt @Double $ fromIntegral (maxBound :: Int)
      evaluate (checkedMultiply n n) `shouldThrow` errorCall "size overflow"
