{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid (module Solid) where

import Solid.Common as Solid
import Solid.Exception as Solid
import IO as Solid (Handle, readFile, writeFile, readBinaryFile, writeBinaryFile, stdin, stdout, stderr)
import Solid.Char ()
import Solid.MD5 as Solid
import FilePath as Solid (FilePath, (</>), (<.>))
import Solid.ToString as Solid

import ByteString as Solid (ByteString)
import String as Solid (String, pack, unpack, words, unwords, lines, unlines)
import Maybe ()
import List ()

print :: ToString a => a -> IO ()
print = stdout.print
