{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
module IO.HandleSpec (spec) where

import Helper

use IO.Handle

number :: Int
number = 23

string :: String
string = "foo"

withHandle :: ByteString -> (Handle -> IO a) -> IO a
withHandle input action = do
  Temp.withDirectory $ \ dir -> do
    let file = dir </> "foo.txt"
    writeBinaryFile file input
    with (file.open IO.ReadMode) action

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

  describe "getContents" $ do
    it "returns the contents of a file" $ do
      withHandle "foobar" Handle.getContents `shouldReturn` "foobar"

  describe "get" $ do
    it "returns n bytes" $ do
      withHandle "foobar" (Handle.get 3) `shouldReturn` "foo"

    context "when less than n bytes are available" $ do
      it "returns all available bytes" $ do
        withHandle "foobar" (Handle.get 10) `shouldReturn` "foobar"

    context "on EOF" $ do
      it "returns the empty string" $ do
        withHandle "" (Handle.get 3) `shouldReturn` ""

  describe "getByte" $ do
    it "returns a single byte" $ do
      withHandle [65] Handle.getByte `shouldReturn` Just 65

    context "on EOF" $ do
      it "returns Nothing" $ do
        withHandle "" Handle.getByte `shouldReturn` Nothing

  describe "getChar" $ do
    it "returns a single character" $ do
      withHandle [65] Handle.getChar `shouldReturn` Just 'A'

    it "decodes UTF-8" $ do
      withHandle [0xF0, 0x9F, 0x91, 0x8B] Handle.getChar `shouldReturn` Just '👋'

    context "on invalid UTF-8" $ do
      it "throws an exception" $ do
        withHandle [0xF0, 0x9F, 0x91] Handle.getChar `shouldThrow` UnicodeDecodeError

    context "on EOF" $ do
      it "returns Nothing" $ do
        withHandle "" Handle.getChar `shouldReturn` Nothing
