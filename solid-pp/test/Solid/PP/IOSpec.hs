{-# LANGUAGE OverloadedStrings #-}
module Solid.PP.IOSpec (spec) where

import           Prelude ()
import           Solid.PP.IO

import           Test.Hspec

spec :: Spec
spec = do
  describe "decodeUtf8" $ do
    context "on invalid UTF-8 sequence" $ do
      it "throws an exception" $ do
        decodeUtf8 "input.txt" "\xc3\x28" `shouldThrow` (== ErrorCall "Encountered an invalid UTF-8 sequence while reading the file \"input.txt\".")
