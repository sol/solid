{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.IO (
  print
, Handle
, stdin
, stdout
, stderr
) where

import           Prelude ()
import           Solid.Common

import           System.IO (Handle, stdin, stdout, stderr)

import           Data.Coerce
import qualified Data.ByteString as B

import           Solid.String
import           Solid.ToString

print :: ToString a => a -> IO ()
print = stdout.print

write :: Handle -> String -> IO ()
write = coerce B.hPut

writeLine :: Handle -> String -> IO ()
writeLine self str = do
  self.write str
  self.write "\n"

instance (HasField "print" Handle (a -> IO ()), ToString a) => HasField "print" Handle (a -> IO ()) where
  getField self = self.writeLine . toString

instance HasField "write" Handle (String -> IO ()) where
  getField = write

instance HasField "writeLine" Handle (String -> IO ()) where
  getField = writeLine
