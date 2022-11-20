{-# LANGUAGE ViewPatterns #-}
module Solid.IOSpec (spec) where

import           Prelude ()
import           Helper

import           System.IO.Silently

number :: Int
number = 23

string :: String
string = "foo"

invalidUtf8 :: ByteString
invalidUtf8 = Bytes "foo \xc3\x28 bar"

file :: FilePath
file = "foo.txt"

spec :: Spec
spec = do
  describe "readFile" $ around_ inTempDirectory $ do
    context "on invalid input" $ do
      it "throws an exception" $ do
        writeBinaryFile file invalidUtf8
        readFile file `shouldThrow` UnicodeDecodeError

    context "when file does not exist" $ do
      it "throws an exception" $ do
        readFile file `shouldThrow` FileNotFoundError file

    context "when file is a directory" $ do
      it "throws an exception" $ do
        touch $ file </> "bar"
        readFile file `shouldThrow` isADirectoryError file

  describe "writeFile" $ around_ inTempDirectory $ do
    it "writes a file to disk" $ do
      property $ \ (pack -> xs) -> do
        writeFile file xs
        readFile file `shouldReturn` xs

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
