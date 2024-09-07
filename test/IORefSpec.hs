{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module IORefSpec (spec) where

import Helper

use IORef

spec :: Spec
spec = do
  describe "write" $ do
    it "writes a new value" $ do
      ref <- IORef.new @Int 23
      ref.write 42
      ref.read `shouldReturn` 42

  describe "modify" $ do
    it "modifies the existing value" $ do
      ref <- IORef.new @Int 23
      ref.modify (+ 42)
      ref.read `shouldReturn` 65
