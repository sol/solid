{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module IO.Handle (
  Handle
, stdin
, stdout
, stderr

, SeekMode(..)

, print
, write
, writeLine
, open?
, close
, flush
, tell
, seek
, rewind
) where

import Solid.Common
import Solid.Types

import           System.IO (SeekMode(..), Handle, stdin, stdout, stderr)
import qualified System.IO as Haskell

import           Data.Coerce
import qualified Data.ByteString as B

import           ByteString ()
import           Solid.ToString

print :: ToString a => Handle -> a -> IO ()
print h = writeLine h . toString

write :: Handle -> String -> IO ()
write = coerce B.hPut

writeLine :: Handle -> String -> IO ()
writeLine self str = do
  self.write str
  self.write "\n"

open? :: Handle -> IO Bool
open? = Haskell.hIsOpen

close :: Handle -> IO ()
close = Haskell.hClose

flush :: Handle -> IO ()
flush = Haskell.hFlush

tell :: Handle -> IO Integer
tell = Haskell.hTell

seek :: Handle -> SeekMode -> Integer -> IO ()
seek = Haskell.hSeek

rewind :: Handle -> IO ()
rewind h = seek h AbsoluteSeek 0

instance (ToString a, HasField "print" Handle (a -> IO ()))
                   => HasField "print" Handle (a -> IO ()) where
  getField = print

instance HasField "write" Handle (String -> IO ()) where
  getField = write

instance HasField "writeLine" Handle (String -> IO ()) where
  getField = writeLine

instance HasField "open\660" Handle (IO Bool) where
  getField = open?

instance HasField "close" Handle (IO ()) where
  getField = close

instance HasField "flush" Handle (IO ()) where
  getField = flush

instance HasField "tell" Handle (IO Integer) where
  getField = tell

instance HasField "seek" Handle (SeekMode -> Integer -> IO ()) where
  getField = seek

instance HasField "rewind" Handle (IO ()) where
  getField = rewind
