{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE ViewPatterns #-}
module IOSpec (spec) where

import Helper
import Gen qualified
import Range qualified

invalidUtf8 :: ByteString
invalidUtf8 = "foo " <> ByteString.pack [0xC3, 0x28] <> " bar"

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
      xs <- forAll $ Gen.list (Range.linear 0 100) Gen.unicodeScalar
      evalIO (writeFile file xs.pack)
      evalIO (readFile file) >>= (=== xs) . unpack
