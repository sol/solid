{-# LANGUAGE ViewPatterns #-}
module Solid.IOSpec (spec) where

import           Prelude ()
import           Solid

import           Test.Hspec
import           Test.QuickCheck
import           Test.Mockery.Directory
import           System.IO.Silently

number :: Int
number = 23

string :: String
string = "foo"

invalidUtf8 :: ByteString
invalidUtf8 = Bytes "foo \xc3\x28 bar"

spec :: Spec
spec = do
  describe "readFile" $ around_ inTempDirectory $ do
    context "on invalid input" $ do
      it "throws an exception" $ do
        writeBinaryFile "foo.txt" invalidUtf8
        readFile "foo.txt" `shouldThrow` (== UnicodeDecodeError)

  describe "writeFile" $ around_ inTempDirectory $ do
    it "writes a file to disk" $ do
      property $ \ (pack -> xs) -> do
        writeFile "foo.txt" xs
        readFile "foo.txt" `shouldReturn` xs

  describe "print" $ do
    it "prints a number " $ do
      capture_ (print number) `shouldReturn` "23\n"

    it "prints a string" $ do
      capture_ (print string) `shouldReturn` "foo\n"

  describe "Handle" $ do
    describe ".print" $ do
      it "prints a number " $ do
        capture_ (stdout.print number) `shouldReturn` "23\n"

      it "prints a string" $ do
        capture_ (stdout.print string) `shouldReturn` "foo\n"

    describe ".write" $ do
      it "writes a string to a Handle" $ do
        capture_ (stdout.write "foo") `shouldReturn` "foo"

    describe ".writeLine" $ do
      it "writes a string and a newline to a Handle" $ do
        capture_ (stdout.writeLine "foo") `shouldReturn` "foo\n"
