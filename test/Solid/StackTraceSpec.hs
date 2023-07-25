{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.StackTraceSpec (spec) where

import Helper hiding (empty)

import ExceptionSpec qualified

import Solid.StackTrace (empty)
import Solid.StackTrace qualified as StackTrace

foo :: WithStackTrace => StackTrace
foo = StackTrace.retrieve

bar :: WithStackTrace => StackTrace
bar = foo

baz :: WithStackTrace => StackTrace
baz = bar

spec :: Spec
spec = do
  describe "toString" $ do
    context "when empty" $ do
      it "returns the empty string" $ do
        empty.toString `shouldBe` ""
        StackTrace.toString empty `shouldBe` ""

    context "when non-empty" $ do
      it "converts to a String" $ do
        let
          expected = List.join "\n" [
              "StackTrace (from WithStackTrace):"
            , "  foo, called at test/ExceptionSpec.hs:19:11 in main:ExceptionSpec"
            , "  bar, called at test/ExceptionSpec.hs:22:11 in main:ExceptionSpec"
            , "  baz, called at test/ExceptionSpec.hs:25:13 in main:ExceptionSpec"
            ]
        ExceptionSpec.trace.toString `shouldBe` expected
        StackTrace.toString ExceptionSpec.trace `shouldBe` expected

  describe "retrieve" $ do
    it "retrieves a StackTrace that has one entry for each WithStackTrace constraint" $ do
      baz.size `shouldBe` 3

    context "without any WithStackTrace constraints" $ do
      it "retrieves an empty StackTrace" $ do
        StackTrace.retrieve `shouldBe` empty

  describe "suppress" $ do
    it "performs some computation without adding any new entries to the StackTrace" $ do
      let
        trace :: WithStackTrace => StackTrace
        trace = StackTrace.suppress baz
      trace.size `shouldBe` 1

  describe "empty?" $ do
    context "when empty" $ do
      it "returns True" $ do
        empty.empty? `shouldBe` True
        StackTrace.empty? empty `shouldBe` True

    context "when non-empty" $ do
      it "returns False" $ do
        foo.empty? `shouldBe` False
        StackTrace.empty? foo `shouldBe` False

  describe "size" $ do
    context "when empty" $ do
      it "returns 0" $ do
        empty.size `shouldBe` 0
        StackTrace.size empty `shouldBe` 0

    context "when non-empty" $ do
      it "returns the number of entries" $ do
        baz.size `shouldBe` 3
        StackTrace.size baz `shouldBe` 3

  describe "pop" $ do
    context "when empty" $ do
      it "is the identity" $ do
        empty.pop `shouldBe` empty
        StackTrace.pop empty `shouldBe` empty

    context "when non-empty" $ do
      it "removes the most recent entry" $ do
        baz.pop.callSites `shouldBe` ["bar", "baz"]
        (StackTrace.pop baz).callSites `shouldBe` ["bar", "baz"]

  describe "callSites" $ do
    it "returns a list of call sites" $ do
      baz.callSites `shouldBe` ["foo", "bar", "baz"]
      StackTrace.callSites baz `shouldBe` ["foo", "bar", "baz"]
