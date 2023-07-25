{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module EitherSpec (spec) where

import Helper

left :: Either String Int
left = Left "foo"

right :: Either String Int
right = Right 23

spec :: Spec
spec = do
  describe "fold" $ do
    context "with a Left-value" $ do
      let value = left
      it "projects the value" $ do
        value.fold id undefined `shouldBe` "foo"
        Either.fold id undefined value `shouldBe` "foo"

    context "with a Right-value" $ do
      let value = right
      it "projects the value" $ do
        value.fold undefined id `shouldBe` 23
        Either.fold undefined id value `shouldBe` 23

  describe "left_or" $ do
    context "with a Left-value" $ do
      let value = left
      it "returns the value" $ do
        value.left_or "bar" `shouldBe` "foo"
        Either.left_or "bar" value `shouldBe` "foo"

    context "with a Right-value" $ do
      let value = right
      it "returns the fallback" $ do
        value.left_or "bar" `shouldBe` "bar"
        Either.left_or "bar" value `shouldBe` "bar"

  describe "right_or" $ do
    context "with a Left-value" $ do
      let value = left
      it "returns the fallback" $ do
        value.right_or 42 `shouldBe` 42
        Either.right_or 42 value `shouldBe` 42

    context "with a Right-value" $ do
      let value = right
      it "returns the value" $ do
        value.right_or 42 `shouldBe` 23
        Either.right_or 42 value `shouldBe` 23

  describe "left!" $ do
    context "with a Left-value" $ do
      let value = left
      it "returns the value" $ do
        value.left! `shouldBe` "foo"
        Either.left! value `shouldBe` "foo"

    context "with a Right-value" $ do
      let value = right
      it "throws an exception" $ do
        evaluate value.left! `shouldThrow?` invalidValue ["Either.left!"] "Right"
        evaluate (Either.left! value) `shouldThrow?` invalidValue ["left!"] "Right"

  describe "right!" $ do
    context "with a Left-value" $ do
      let value = left
      it "throws an exception" $ do
        evaluate value.right! `shouldThrow?` invalidValue ["Either.right!"] "Left"
        evaluate (Either.right! value) `shouldThrow?` invalidValue ["right!"] "Left"

    context "with a Right-value" $ do
      let value = right
      it "returns the value" $ do
        value.right! `shouldBe` 23
        Either.right! value `shouldBe` 23

  describe "left?" $ do
    context "with a Left-value" $ do
      let value = left
      it "returns True" $ do
        value.left? `shouldBe` True
        Either.left? value `shouldBe` True

    context "with a Right-value" $ do
      let value = right
      it "returns False" $ do
        value.left? `shouldBe` False
        Either.left? value `shouldBe` False

  describe "right?" $ do
    context "with a Left-value" $ do
      let value = left
      it "returns False" $ do
        value.right? `shouldBe` False
        Either.right? value `shouldBe` False

    context "with a Right-value" $ do
      let value = right
      it "returns True" $ do
        value.right? `shouldBe` True
        Either.right? value `shouldBe` True
