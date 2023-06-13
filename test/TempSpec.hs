{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module TempSpec (spec) where

import Helper

spec :: Spec
spec = do
  describe "withDirectory" $ do
    it "creates a temporary directory and passes it to an action" $ do
      Temp.withDirectory $ \ dir -> do
        dir.directory? `shouldReturn` True

    context "when done" $ do
      it "removes the temporary directory" $ do
        dir <- Temp.withDirectory return
        dir.exists? `shouldReturn` False

      context "when the temporary directory is not empty" $ do
        it "removes the temporary directory" $ do
          dir <- Temp.withDirectory $ \ dir -> do
            touch $ dir </> "foo"
            return dir
          dir.exists? `shouldReturn` False

  describe "withFile" $ do
    it "creates a temporary file and passes it to an action" $ do
      Temp.withFile $ \ name h -> do
        name.file? `shouldReturn` True
        h.open? `shouldReturn` True

    context "when done" $ do
      it "removes the temporary file" $ do
        (name, h) <- Temp.withFile $ curry return
        name.exists? `shouldReturn` False
        h.open? `shouldReturn` False

  describe "writeFileAt" $ do
    it "writes a temporary file" $ do
      Temp.withDirectory $ \ dir -> do
        file <- Temp.writeFileAt dir "foo"
        readFile file `shouldReturn` "foo"
