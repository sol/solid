module Solid.PP.Lexer.FastSpec (spec) where

import           Test.Hspec
import           Control.Monad
import           Data.ByteString.Internal (c2w, w2c)
import           Data.List

import           Solid.PP.Lexer.Fast as Fast

spec :: Spec
spec = do
  let symbols = map c2w $ sort ":!#$%&*+./<=>?@\\^|-~"
  describe "isSymbol" $ do
    forM_ symbols $ \ c -> do
      it [w2c c] $ do
        Fast.isSymbol c `shouldBe` True

    forM_ ([minBound .. maxBound] \\ symbols) $ \ c -> do
      it (show c) $ do
        Fast.isSymbol c `shouldBe` False
