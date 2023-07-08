{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid (module Solid) where

import Solid.Common as Solid
import Exception as Solid hiding (handle)
import IO as Solid (readFile, writeFile, readBinaryFile, writeBinaryFile, stdin, stdout, stderr)
import Solid.Char ()
import Solid.MD5 as Solid
import FilePath as Solid (FilePath, (</>), (<.>))
import Solid.ToString as Solid

import Solid.Types as Solid (Bytes)
import ByteString as Solid (ByteString)
import String as Solid (String, pack, unpack, words, unwords, lines, unlines)
import Solid.Ansi ()
import Maybe ()
import List ()

import Numeric qualified

print :: ToString a => a -> IO ()
print = stdout.print

putStrLn :: String -> IO ()
putStrLn = stdout.writeLine

showHex :: Integral a => a -> String
showHex n = pack $ Numeric.showHex n ""

instance HasField "showHex" Word8 String where
  getField = showHex

instance HasField "showHex" Word16 String where
  getField = showHex

instance HasField "showHex" Word32 String where
  getField = showHex

instance HasField "showHex" Word64 String where
  getField = showHex

instance HasField "showHex" Int String where
  getField = showHex

instance HasField "showHex" Integer String where
  getField = showHex
