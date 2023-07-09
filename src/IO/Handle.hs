{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module IO.Handle (
  Handle
, stdin
, stdout
, stderr

, Mode
, Haskell.IOMode(..)
, SeekMode(..)

, print
, write
, writeLine
, tty?
, open?
, open
, close
, flush
, tell
, seek
, rewind
, getContents
, withLock
) where

import Solid.Common
import Solid.Types

import           System.IO (SeekMode(..), stdin, stdout, stderr)
import qualified System.IO as Haskell
import qualified System.File.OsPath as OsPath

import           Data.Coerce
import qualified Data.ByteString as B

import           ByteString ()
import           Solid.ToString

import           GHC.IO.Handle.Types (Handle(..))
import           Control.Concurrent.MVar (withMVar)

type Mode = Haskell.IOMode

print :: ToString a => Handle -> a -> IO ()
print h = writeLine h . toString

write :: Handle -> String -> IO ()
write = coerce B.hPut

writeLine :: Handle -> String -> IO ()
writeLine self str = do
  self.write str
  self.write "\n"

tty? :: Handle -> IO Bool
tty? = Haskell.hIsTerminalDevice

open? :: Handle -> IO Bool
open? = Haskell.hIsOpen

open :: FilePath -> Mode -> IO Handle
open = OsPath.openFile . coerce

close :: Handle -> IO ()
close = Haskell.hClose

flush :: Handle -> IO ()
flush = Haskell.hFlush

tell :: Handle -> IO Integer
tell = Haskell.hTell

seek :: SeekMode -> Integer -> Handle -> IO ()
seek mode n h = Haskell.hSeek h mode n

rewind :: Handle -> IO ()
rewind = seek AbsoluteSeek 0

getContents :: Handle -> IO ByteString
getContents = fmap Bytes . B.hGetContents

withLock :: IO a -> Handle -> IO a
withLock action h = withMVar handle__ $ \ _ -> action
  where
    handle__ = case h of
      FileHandle _ m -> m
      DuplexHandle _ m _ -> m

instance (ToString a, HasField "print" Handle (a -> IO ()))
                   => HasField "print" Handle (a -> IO ()) where
  getField = print

instance HasField "write" Handle (String -> IO ()) where
  getField = write

instance HasField "writeLine" Handle (String -> IO ()) where
  getField = writeLine

instance HasField "tty\660" Handle (IO Bool) where
  getField = tty?

instance HasField "open\660" Handle (IO Bool) where
  getField = open?

instance HasField "close" Handle (IO ()) where
  getField = close

instance HasField "release" Handle (IO ()) where
  getField = close

instance HasField "flush" Handle (IO ()) where
  getField = flush

instance HasField "tell" Handle (IO Integer) where
  getField = tell

instance HasField "seek" Handle (SeekMode -> Integer -> IO ()) where
  getField h mode n = seek mode n h

instance HasField "rewind" Handle (IO ()) where
  getField = rewind

instance HasField "getContents" Handle (IO ByteString) where
  getField = getContents

instance HasField "withLock" Handle (IO a -> IO a) =>
         HasField "withLock" Handle (IO a -> IO a) where
  getField = flip withLock
