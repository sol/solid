{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module CharSpec (spec) where

import Helper
import Gen

spec :: Spec
spec = do
  describe ".ascii?" $ do
    it "returns True for ASCII characters" $ do
      c <- forAll Gen.ascii
      c.ascii? === True

    it "returns False for non-ASCII characters" $ do
      c <- forAll $ Gen.enum '\128' maxBound
      c.ascii? === False

  describe ".valid?" $ do
    it "returns True for Unicode scalars" $ do
      c <- forAll Gen.unicodeScalar
      c.valid? === True

    it "returns False for surrogates" $ do
      c <- forAll Gen.surrogate
      c.valid? === False

  describe ".ord" $ do
    it "is inverse to chr" $ do
      c <- forAll Gen.unicodeAny
      Char.chr c.ord === c
