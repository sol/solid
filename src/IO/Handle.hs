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
, BufferMode(..)

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
, setEcho
, setBuffering
, getContents
, withLock
) where

import Solid.Common
import Solid.String
import Solid.ByteString
import Solid.Bytes.Unsafe

import           System.IO (SeekMode(..), BufferMode(..), stdin, stdout, stderr)
import qualified System.IO as Haskell
import qualified System.File.OsPath as OsPath

import           Data.Coerce
import qualified Data.ByteString as B

import           ByteString ()
import           Solid.ToString

import           GHC.IO.Handle.Types (Handle(..))
import           Control.Concurrent.MVar (withMVar)

use Haskell

type Mode = Haskell.IOMode

print :: ToString a => Handle -> a -> IO ()
print h = writeLine h . toString

write :: Handle -> String -> IO ()
write h = B.hPut h . Haskell.asByteString

writeLine :: Handle -> String -> IO ()
writeLine self str = do
  self.write str
  self.write "\n"

.tty? :: Handle -> IO Bool
.tty? = Haskell.hIsTerminalDevice

.open? :: Handle -> IO Bool
.open? = Haskell.hIsOpen

open :: FilePath -> Mode -> IO Handle
open = OsPath.openFile . coerce

.close :: Handle -> IO ()
.close = Haskell.hClose

.flush :: Handle -> IO ()
.flush = Haskell.hFlush

.tell :: Handle -> IO Integer
.tell = Haskell.hTell

.seek :: SeekMode -> Integer -> Handle -> IO ()
.seek mode n h = Haskell.hSeek h mode n

.rewind :: Handle -> IO ()
.rewind = seek AbsoluteSeek 0

.setEcho :: Bool -> Handle -> IO ()
.setEcho = flip Haskell.hSetEcho

.setBuffering :: BufferMode -> Handle -> IO ()
.setBuffering = flip Haskell.hSetBuffering

.getContents :: Handle -> IO ByteString
.getContents = fmap Haskell.fromByteString . B.hGetContents

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

instance HasField "release" Handle (IO ()) where
  getField = close

instance HasField "withLock" Handle (IO a -> IO a) =>
         HasField "withLock" Handle (IO a -> IO a) where
  getField = flip withLock
