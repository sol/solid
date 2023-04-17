module Solid.BindSpec (spec) where

import Helper

int :: IO Int
int = return 23

str :: IO String
str = return "foo"

spec :: Spec
spec = do
  describe "(-<)" $ do
    it "binds" $ do
      let action a b = return (a, b)
      (action -< int -< str) `shouldReturn` (23, "foo")
