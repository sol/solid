{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-x-partial #-}
module EndToEndSpec (spec) where

import           Test.Hspec

import           GHC.Records
import           Data.Char

instance HasField "toLower" String String where
  getField = map toLower

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

    it "desugars nested string interpolation" $ do
      let
        foo = "foo"
        bar = "bar"
      " { " { " {foo} " } " } {bar} " `shouldBe` "   foo   bar "

    it "desugars interpolation abstractions" $ do
      let
        how = "how"
        are = "are"
        you = "you"
      "Hey {name}, {} {} {}?" how are you `shouldBe` "Hey Joe, how are you?"

    context "workaround for GHC #23040" $ do
      -- https://gitlab.haskell.org/ghc/ghc/-/issues/23040
      it "desugars string interpolation" $ do
        "Hey {name} 👋".toLower `shouldBe` "hey joe 👋"

    it "desugars identifiers that end with a bang" $ do
      head! [] `shouldThrow` errorCall "Prelude.head: empty list"

    it "desugars identifiers that end with a question mark" $ do
      empty? [] `shouldBe` True
