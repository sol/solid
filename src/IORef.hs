{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module IORef (
  IORef
, new
, read
, write
, modify

, Atomic
, atomic
) where

import Prelude hiding (read)
import Data.IORef
import IORef.Atomic (Atomic)
use IORef.Atomic

-- all operations are strict, if we ever need lazy versions then introduce
--
-- .write~
-- .swap~
--
-- etc

new :: a -> IO (IORef a)
new = newIORef

.read :: IORef a -> IO a
.read = readIORef

.write :: a -> IORef a -> IO ()
.write !a ref = writeIORef ref a

.modify :: (a -> a) -> IORef a -> IO ()
.modify f ref = modifyIORef' ref f

.atomic :: IORef a -> Atomic a
.atomic = Atomic.Atomic
