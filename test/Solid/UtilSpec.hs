{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.UtilSpec (spec) where

import           Helper

import           Solid.Util

spec :: Spec
spec = do
  describe "createAtomic" $ do
    around Temp.withDirectory $ do
      it "populates a directory atomically" $ \ sandbox -> do
        let dst = sandbox </> "foo"
        createAtomic dst $ \ tmp -> do
          touch (tmp </> "bar")
        (dst </> "bar").exists? `shouldReturn` True

      context "when already exists" $ do
        it "keeps the existing directory" $ \ sandbox -> do
          let dst = sandbox </> "foo"
          touch (dst </> "bar")
          createAtomic dst $ \ _ -> pass
          (dst </> "bar").exists? `shouldReturn` True
