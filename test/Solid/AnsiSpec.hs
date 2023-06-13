{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.AnsiSpec (spec) where

import Helper

spec :: Spec
spec = do
  describe "bold" $ do
    it "sets bold mode" $ do
      let input = "bar" :: String
      "foo {input.ansi.bold} baz" `shouldBe` "foo \ESC[1mbar\ESC[22m baz"

  describe "underline" $ do
    it "sets underline mode" $ do
      let input = "bar" :: String
      "foo {input.ansi.underline} baz" `shouldBe` "foo \ESC[4mbar\ESC[24m baz"

  describe "inverse" $ do
    it "sets inverse video mode" $ do
      let input = "bar" :: String
      "foo {input.ansi.inverse} baz" `shouldBe` "foo \ESC[7mbar\ESC[27m baz"

  describe "black" $ do
    it "sets the foreground color to black" $ do
      let input = "bar" :: String
      "foo {input.ansi.black} baz" `shouldBe` "foo \ESC[30mbar\ESC[39m baz"

  describe "red" $ do
    it "sets the foreground color to red" $ do
      let input = "bar" :: String
      "foo {input.ansi.red} baz" `shouldBe` "foo \ESC[31mbar\ESC[39m baz"

  describe "green" $ do
    it "sets the foreground color to green" $ do
      let input = "bar" :: String
      "foo {input.ansi.green} baz" `shouldBe` "foo \ESC[32mbar\ESC[39m baz"

  describe "yellow" $ do
    it "sets the foreground color to yellow" $ do
      let input = "bar" :: String
      "foo {input.ansi.yellow} baz" `shouldBe` "foo \ESC[33mbar\ESC[39m baz"

  describe "blue" $ do
    it "sets the foreground color to blue" $ do
      let input = "bar" :: String
      "foo {input.ansi.blue} baz" `shouldBe` "foo \ESC[34mbar\ESC[39m baz"

  describe "magenta" $ do
    it "sets the foreground color to magenta" $ do
      let input = "bar" :: String
      "foo {input.ansi.magenta} baz" `shouldBe` "foo \ESC[35mbar\ESC[39m baz"

  describe "cyan" $ do
    it "sets the foreground color to cyan" $ do
      let input = "bar" :: String
      "foo {input.ansi.cyan} baz" `shouldBe` "foo \ESC[36mbar\ESC[39m baz"

  describe "white" $ do
    it "sets the foreground color to white" $ do
      let input = "bar" :: String
      "foo {input.ansi.white} baz" `shouldBe` "foo \ESC[37mbar\ESC[39m baz"

  describe "rgb" $ do
    it "sets the foreground color to a specific rgb value" $ do
      let input = "bar" :: String
      "foo {input.ansi.rgb 64 128 maxBound} baz" `shouldBe` "foo \ESC[38;2;64;128;255mbar\ESC[39m baz"

  context "with multiple properties" $ do
    it "combines all properties" $ do
      let input = "bar" :: String
      toString input.ansi.red.bold.underline `shouldBe` "\ESC[4;1;31mbar\ESC[39;22;24m"
