{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module MaybeSpec (spec) where

import           Helper

nothing :: Maybe Int
nothing = Nothing

just :: Maybe Int
just = Just 23

spec :: Spec
spec = do
  describe ".nothing?" $ do
    context "with a Nothing" $ do
      it "returns True" $ do
        nothing.nothing? `shouldBe` True

    context "with a Just" $ do
      it "returns False" $ do
        just.nothing? `shouldBe` False

  describe ".just?" $ do
    context "with a Just" $ do
      it "returns True" $ do
        just.just? `shouldBe` True

    context "with a Nothing" $ do
      it "returns False" $ do
        nothing.just? `shouldBe` False
