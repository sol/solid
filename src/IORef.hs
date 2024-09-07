{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module IORef (
  IORef
, new
, read
, write
, modify
) where

import Prelude hiding (read)
import Data.IORef

new :: a -> IO (IORef a)
new = newIORef

.read :: IORef a -> IO a
.read = readIORef

.write :: a -> IORef a -> IO ()
.write !a ref = writeIORef ref a

.modify :: (a -> a) -> IORef a -> IO ()
.modify f ref = modifyIORef' ref f
