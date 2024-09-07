{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module IORef.Atomic where

import Solid.Common

import Data.IORef
import GHC.IORef

newtype Atomic a = Atomic (IORef a)
  deriving newtype Eq

new :: a -> IO (Atomic a)
new = fmap Atomic . newIORef

.read :: Atomic a -> IO a
.read (Atomic ref) = atomicModifyIORef' ref (id &&& id)

.write :: a -> Atomic a -> IO ()
.write !a (Atomic ref) = atomicWriteIORef ref a

.swap :: a -> Atomic a -> IO a
.swap !a (Atomic ref) = atomicSwapIORef ref a

.modify_read :: (a -> a) -> Atomic a -> IO a
.modify_read f = modify $ \ (f -> a) -> (a, a)

.modify_ :: (a -> a) -> Atomic a -> IO ()
.modify_ f (Atomic ref) = void $ atomicModifyIORef'_ ref f

.modify :: (a -> (a, b)) -> Atomic a -> IO b
.modify f (Atomic ref) = atomicModifyIORef' ref f
