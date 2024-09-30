{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module ExceptionSpec (spec, trace) where

import Helper

import GHC.Stack
import Solid.StackTrace qualified as StackTrace

loc :: Int -> Int -> SrcLoc
loc line column = SrcLoc "main" "ExceptionSpec" "test/ExceptionSpec.hs" line column line column

trace :: StackTrace
trace = StackTrace.fromCallStack $ fromCallSiteList [
    ("foo", loc 19 11)
  , ("bar", loc 22 11)
  , ("baz", loc 25 13)
  ]

spec :: Spec
spec = do
  describe "InvalidValue" $ do
    describe "toString" $ do
      context "without a StackTrace" $ do
        it "converts to a String" $ do
          (Exception.InvalidValue StackTrace.empty "foo").toString `shouldBe` "InvalidValue: foo"

      context "with a StackTrace" $ do
        it "converts to a String" $ do
          (Exception.InvalidValue trace "foo").toString `shouldBe` String.join "\n" [
              ""
            , ""
            , "StackTrace (from WithStackTrace):"
            , "  foo, called at test/ExceptionSpec.hs:19:11 in main:ExceptionSpec"
            , "  bar, called at test/ExceptionSpec.hs:22:11 in main:ExceptionSpec"
            , "  baz, called at test/ExceptionSpec.hs:25:13 in main:ExceptionSpec"
            , ""
            , "InvalidValue: foo"
            ]
