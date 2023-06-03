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

  describe "path" $ do
    describe "extend" $ do
      context "when PATH is not set" $ do
        it "sets the PATH" $ do
          Env.unset "PATH"
          Env.path.extend "/foo/bar" $ do
            Env.get "PATH" `shouldReturn` Just "/foo/bar"
          Env.get "PATH" `shouldReturn` Nothing

      context "when PATH is set" $ do
        it "extends the PATH" $ do
          path <- Env.get "PATH"
          Env.path.extend "/foo/bar" $ do
            (>>= flip (.stripPrefix) "/foo/bar:") <$> Env.get "PATH" `shouldReturn` path
          Env.get "PATH" `shouldReturn` path
