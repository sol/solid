{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module EndToEndSpec (spec) where

import           Test.Hspec

toString :: String -> String
toString = id

name :: String
name = "Joe"

head! :: [a] -> a
head! = head

empty? :: [a] -> Bool
empty? = null

spec :: Spec
spec = do
  describe "solid-pp" $ do
    it "desugars string interpolation" $ do
      "Hey {name} 👋" `shouldBe` "Hey Joe 👋"

    it "desugars identifiers that end with a bang" $ do
      head! [] `shouldThrow` errorCall "Prelude.head: empty list"

    it "desugars identifiers that end with a question mark" $ do
      empty? [] `shouldBe` True
