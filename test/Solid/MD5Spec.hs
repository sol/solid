module Solid.MD5Spec (spec) where

import           Helper

foo :: String
foo = "foo"

bar :: String
bar = "bar"

baz :: String
baz = "baz"

spec :: Spec
spec = do
  describe ".md5sum @String" $ do
    it "calculates the MD5 sum of a String" $ do
      foo.md5sum.toString `shouldBe` "acbd18db4cc2f85cedef654fccc4a4d8"

  describe ".md5sum @[Fingerprint]" $ do
    it "combines a list of MD5 sums to a single MD5 sum" $ do
      (map (.md5sum) [foo, bar, baz]).md5sum.toString `shouldBe` "0061f2cf4496958fdb3a6ae47684dbb0"

  describe ".md5sum @FilePath" $ around_ inTempDirectory $ do
    let
      file :: FilePath
      file = "foo.txt"

    it "calculates the MD5 sum of a file" $ do
      writeFile file "foo"
      toString <$> file.md5sum `shouldReturn` foo.md5sum.toString

    context "when file does not exist" $ do
      it "throws an exception" $ do
        file.md5sum `shouldThrow` FileNotFoundError file

    context "when file is a directory" $ do
      it "throws an exception" $ do
        touch $ file </> "bar"
        file.md5sum `shouldThrow` isADirectoryError file
