{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module ListSpec (spec) where

import Helper

spec :: Spec
spec = do
  describe "empty?" $ do
    context "with the empty list" $ do
      it "returns True" $ do
        [].empty? `shouldBe` True
        List.empty? [] `shouldBe` True

  describe "map" $ do
    it "maps over a list" $ do
      [1..3].map succ `shouldBe` [2..4 :: Int]
      List.map succ [1..3] `shouldBe` [2..4 :: Int]

  describe ".startsWith" $ do
    it "checks if a string starts with an other string" $ do
      let input = [1, 2, 3] :: [Int]
      input.startsWith [1] `shouldBe` True

  describe ".endsWith" $ do
    it "checks if a string ends with an other string" $ do
      let input = [1, 2, 3] :: [Int]
      input.endsWith [3] `shouldBe` True

  describe ".contains" $ do
    it "checks if a string contains an other string" $ do
      let input = [1, 2, 3] :: [Int]
      input.contains [2] `shouldBe` True

  describe ".stripPrefix" $ do
    it "strips prefix" $ do
      let input = "foobar" :: [Char]
      input.stripPrefix "foo" `shouldBe` Just "bar"

  describe "nub" $ do
    it "removes duplicates from a list" $ do
      [1, 2, 3, 23, 1, 42, 1].nub `shouldBe` [1, 2, 3, 23, 42 :: Int]

  describe "nub!!" $ do
    it "removes duplicates from a list" $ do
      [1, 2, 3, 23, 1, 42, 1].nub!! `shouldBe` [1, 2, 3, 23, 42 :: Int]

  describe "zip" $ do
    it "takes two lists and returns a list of corresponding pairs" $ do
      let
        input = ["foo", "bar", "baz" :: String]
        expected = [(1, "foo"), (2, "bar"), (3, "baz")]
      List.zip [1 :: Int ..] input `shouldBe` expected
      input.zip [1 :: Int ..] `shouldBe` expected

  describe "enumerate" $ do
    it "enumerates a list" $ do
      let
        input = ["foo", "bar", "baz" :: String]
        expected = [(0, "foo"), (1, "bar"), (2, "baz")]
      List.enumerate input `shouldBe` expected
      input.enumerate `shouldBe` expected

  describe "randomChoice" $ do
    it "returns a random list element" $ do
      let
        input = ["foo", "bar", "baz" :: String]
        expected = input.sort
      List.sort . List.nub <$> replicateM 100 (List.randomChoice input) `shouldReturn` expected
      List.sort . List.nub <$> replicateM 100 input.randomChoice `shouldReturn` expected
