{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Helper (
  module Imports
, touch
, capture_
, shouldThrow
, isADirectoryError
) where

import           Solid as Imports
import           Test.Hspec as Imports hiding (shouldThrow)
import           Test.Hspec.Hedgehog as Imports
import           Test.Mockery.Directory as Imports (inTempDirectory)

import System.IO.Silently qualified as Silently
import qualified Test.Mockery.Directory as Mockery

import           Data.Typeable (typeOf)

import Solid.TypeLits
import Solid.Foreign.Haskell qualified as Haskell

touch :: FilePath -> IO ()
touch = Haskell.toFilePath >=> Mockery.touch

capture_ :: IO a -> IO String
capture_ = fmap pack . Silently.capture_

infix 1 `shouldThrow`

shouldThrow :: (HasCallStack, Eq e, Exception e) => IO a -> e -> Expectation
action `shouldThrow` e = do
  r <- try action
  case r of
    Right _ -> expectationFailure $ unpack "did not get expected exception: {typeOf e}"
    Left err -> err `shouldBe` e

isADirectoryError :: FilePath -> IOException
isADirectoryError = if Platform.windows? then PermissionError else IsADirectoryError

data Foo

instance KnownSymbol name => HasField (name :: Symbol) Foo String where
  -- This instance collides with instances of the form:
  --
  --   instance Show a => HasField "show" a String where
  --     getField = show
  --
  -- GHC rejects instances of this form.  This definition protects us from
  -- shooting ourselves in the knee in the unlikely case that GHC ever decides
  -- to lift this restriction.
  getField _ = symbolValue @name
