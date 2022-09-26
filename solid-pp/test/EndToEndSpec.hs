{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module EndToEndSpec (spec) where

import           Test.Hspec

toString :: String -> String
toString = id

name :: String
name = "Joe"

spec :: Spec
spec = do
  describe "solid-pp" $ do
    it "desugars string interpolation" $ do
      "Hey {name} 👋" `shouldBe` "Hey Joe 👋"
