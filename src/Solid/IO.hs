{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.IO (
  print

, readFile
, writeFile

, readBinaryFile
, writeBinaryFile

, Handle
, stdin
, stdout
, stderr
) where

import           Solid.Common
import           Solid.Types
import           Solid.Exception
import           FilePath

import           System.IO (Handle, stdin, stdout, stderr, hFlush)

import           Data.Coerce
import qualified Data.ByteString as B

import           ByteString ()
import           Solid.ToString

print :: ToString a => a -> IO ()
print = stdout.print

readFile :: FilePath -> IO String
readFile file = do
  bytes <- readBinaryFile file
  case bytes.asString of
    Just string -> return string
    Nothing -> throwIO UnicodeDecodeError

writeFile :: FilePath -> String -> IO ()
writeFile = coerce B.writeFile

readBinaryFile :: FilePath -> IO ByteString
readBinaryFile = coerce B.readFile

writeBinaryFile :: FilePath -> Bytes a -> IO ()
writeBinaryFile = coerce B.writeFile

instance (HasField "print" Handle (a -> IO ()), ToString a) => HasField "print" Handle (a -> IO ()) where
  getField self = self.writeLine . toString

instance HasField "flush" Handle (IO ()) where
  getField = hFlush

write :: Handle -> String -> IO ()
write = coerce B.hPut

instance HasField "write" Handle (String -> IO ()) where
  getField = write

writeLine :: Handle -> String -> IO ()
writeLine self str = do
  self.write str
  self.write "\n"

instance HasField "writeLine" Handle (String -> IO ()) where
  getField = writeLine
