module SolidSpec (spec) where

import Helper

number :: Int
number = 23

string :: String
string = "foo"

spec :: Spec
spec = do
  describe "print" $ do
    it "prints a number" $ do
      capture_ (print number) `shouldReturn` "23\n"

    it "prints a string" $ do
      capture_ (print string) `shouldReturn` "foo\n"
