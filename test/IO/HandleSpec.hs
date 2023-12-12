{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
module IO.HandleSpec (spec) where

import Helper

number :: Int
number = 23

string :: String
string = "foo"

withHandle :: ByteString -> (Handle -> IO a) -> IO a
withHandle input action = do
  Temp.withDirectory $ \ dir -> do
    let file = dir </> "foo.txt"
    writeBinaryFile file input
    with (IO.open file IO.ReadMode) action

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

  describe "getByte" $ do
    it "" $ do
      withHandle [65] $ \ handle -> do
        handle.getByte `shouldReturn` Just 65
    it "" $ do
      withHandle [] $ \ handle -> do
        handle.getByte `shouldReturn` Nothing

  describe "getChar" $ do
    it "" $ do
      withHandle [65] $ \ handle -> do
        handle.getChar `shouldReturn` Just 'A'

    it "" $ do
      withHandle [0xF0, 0x9F, 0x91, 0x8B] $ \ handle -> do
        handle.getChar `shouldReturn` Just '👋'

    it "" $ do
      withHandle [0xF0, 0x9F, 0x91] $ \ handle -> do
        handle.getChar `shouldThrow` UnicodeDecodeError

    it "" $ do
      withHandle [] $ \ handle -> do
        handle.getChar `shouldReturn` Nothing
