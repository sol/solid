module Solid.IOSpec (spec) where

import           Prelude ()
import           Solid

import           Test.Hspec
import           System.IO.Silently

number :: Int
number = 23

string :: String
string = "foo"

spec :: Spec
spec = do
  describe "print" $ do
    it "prints a number " $ do
      capture_ (print number) `shouldReturn` "23\n"

    it "prints a string" $ do
      capture_ (print string) `shouldReturn` "foo\n"

  describe "Handle.print" $ do
    it "prints a number " $ do
      capture_ (stdout.print number) `shouldReturn` "23\n"

    it "prints a string" $ do
      capture_ (stdout.print string) `shouldReturn` "foo\n"

  describe "Handle.write" $ do
    it "writes a string to a Handle" $ do
      capture_ (stdout.write "foo") `shouldReturn` "foo"

  describe "Handle.writeLine" $ do
    it "writes a string and a newline to a Handle" $ do
      capture_ (stdout.writeLine "foo") `shouldReturn` "foo\n"
