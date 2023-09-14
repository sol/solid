{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE UndecidableInstances #-}
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
import           Data.Bool (bool)

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

instance HasField "fst" (a, b) a where
  getField = fst

instance HasField "snd" (a, b) b where
  getField = snd

instance HasField "bimap" (a, b) ((a -> c) -> (b -> d) -> (c, d))
      => HasField "bimap" (a, b) ((a -> c) -> (b -> d) -> (c, d)) where
  getField subject f g = bimap f g subject

instance HasField "fold" Bool (a -> a -> a)
      => HasField "fold" Bool (a -> a -> a) where
  getField value t f = bool t f value

instance HasField "pred" Int Int where
  getField = pred

instance HasField "succ" Int Int where
  getField = succ

instance HasField "max" Int (Int -> Int) where
  getField = max

instance HasField "min" Int (Int -> Int) where
  getField = min
