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

import           Prelude ()
import           Solid.Common
import           Solid.Exception

import           System.IO (Handle, stdin, stdout, stderr)

import           Data.Coerce
import qualified Data.ByteString as B

import           Solid.String
import           Solid.ToString

print :: ToString a => a -> IO ()
print = stdout.print

readFile :: FilePath -> IO String
readFile file = do
  bytes <- readBinaryFile file
  case bytes.asString of
    Just string -> return string
    Nothing -> throw UnicodeDecodeError

writeFile :: FilePath -> String -> IO ()
writeFile = coerce B.writeFile

readBinaryFile :: FilePath -> IO ByteString
readBinaryFile = coerce B.readFile

writeBinaryFile :: FilePath -> Bytes a -> IO ()
writeBinaryFile = coerce B.writeFile

instance (HasField "print" Handle (a -> IO ()), ToString a) => HasField "print" Handle (a -> IO ()) where
  getField self = self.writeLine . toString

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
