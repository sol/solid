{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module MaybeSpec (spec) where

import Helper

nothing :: Maybe Int
nothing = Nothing

just :: Maybe Int
just = Just 23

spec :: Spec
spec = do
  describe "fold" $ do
    context "with Nothing" $ do
      let value = nothing
      it "returns the default" $ do
        value.fold 42 id `shouldBe` 42
        Maybe.fold 42 id value `shouldBe` 42

    context "with a Just-value" $ do
      let value = just
      it "projects the value" $ do
        value.fold 42 id `shouldBe` 23
        Maybe.fold 42 id value `shouldBe` 23

  describe "nothing?" $ do
    context "with Nothing" $ do
      let value = nothing
      it "returns True" $ do
        value.nothing? `shouldBe` True
        Maybe.nothing? value `shouldBe` True

    context "with a Just-value" $ do
      let value = just
      it "returns False" $ do
        value.nothing? `shouldBe` False
        Maybe.nothing? value `shouldBe` False

  describe "just?" $ do
    context "with Nothing" $ do
      let value = nothing
      it "returns False" $ do
        value.just? `shouldBe` False
        Maybe.just? value `shouldBe` False

    context "with a Just-value" $ do
      let value = just
      it "returns True" $ do
        value.just? `shouldBe` True
        Maybe.just? value `shouldBe` True
