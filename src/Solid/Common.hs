{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ImplicitParams #-}
module Solid.Common (
  WithStackTrace
, module Imports
, (-<)
, pass
, with
) where

import           HaskellPrelude as Imports hiding (
    String
  , FilePath
  , words
  , unwords
  , lines
  , unlines
  , length

  , print
  , putStrLn
  , readFile
  , writeFile
  , getContents

  , error
  )
import           Data.Function as Imports ((&))
import           Data.Functor as Imports ((<&>))
import           Data.Bifunctor as Imports
import           Data.String as Imports (IsString(..))
import           Data.Word as Imports (Word8, Word16, Word32, Word64)
import           Data.Int as Imports (Int8, Int16, Int32, Int64)
import           System.IO as Imports (Handle)
import           GHC.Records as Imports (HasField(..))
import           GHC.Generics as Imports (Generic)
import           Control.Exception (bracket)
import           Control.Monad as Imports
import           Control.Monad.IO.Class as Imports
import           Control.Arrow as Imports ((>>>))
import           Control.Applicative as Imports

import           Data.Tuple (swap)

import qualified GHC.Stack as GHC

type WithStackTrace = (?callStack :: GHC.CallStack)

class Bind m r where
  bind :: (a -> r) -> m a -> r

instance Monad m => Bind m (m b) where
  bind :: (a -> m b) -> m a -> m b
  bind = (=<<)

instance Bind m r => Bind m (b -> r) where
  bind :: (a -> b -> r) -> m a -> b -> r
  bind f ma b = bind (flip f b) ma

infixl 1 -<

(-<) :: Bind m r => (a -> r) -> m a -> r
(-<) = bind

pass :: Applicative m => m ()
pass = pure ()

with :: HasField "release" resource (IO ()) => IO resource -> (resource -> IO a) -> IO a
with acquire = bracket acquire (.release)

instance HasField "curry" ((a, b) -> c) (a -> b -> c) where
  getField = curry

instance HasField "uncurry" (a -> b -> c) ((a, b) -> c) where
  getField = uncurry

instance HasField "flip" (a -> b -> c) (b -> a -> c) where
  getField = flip

instance HasField "swap" (a, b) (b, a) where
  getField = swap
