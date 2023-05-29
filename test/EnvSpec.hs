module EnvSpec (spec) where

import           Helper

import qualified Env
import qualified Env.Raw

spec :: Spec
spec = around_ Env.protect $ do
  describe "get" $ do
    context "on invalid UTF-8" $ do
      it "uses Unicode replacement characters" $ do
        let invalid = "foo " <> [128 :: Word8].pack <> " bar"
        Env.Raw.set "foo" invalid
        Env.get "foo" `shouldReturn` Just "foo \xFFFD bar"

  describe "extend" $ do
    it "extends the environment" $ do
      Env.extend [("foo", "bar")] $ do
        Env.get "foo" `shouldReturn` Just "bar"
