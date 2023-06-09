{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE ViewPatterns #-}
module IO.HandleSpec (spec) where

import Helper

number :: Int
number = 23

string :: String
string = "foo"

spec :: Spec
spec = do
  describe "print" $ do
    it "prints a number" $ do
      capture_ (stdout.print number) `shouldReturn` "23\n"

    it "prints a string" $ do
      capture_ (stdout.print string) `shouldReturn` "foo\n"

  describe "write" $ do
    it "writes a string to a Handle" $ do
      capture_ (stdout.write "foo") `shouldReturn` "foo"

  describe "writeLine" $ do
    it "writes a string and a newline to a Handle" $ do
      capture_ (stdout.writeLine "foo") `shouldReturn` "foo\n"
