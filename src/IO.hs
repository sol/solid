{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module IO (
  readFile
, writeFile

, readBinaryFile
, writeBinaryFile

, module IO.Handle
) where

import Solid.Common
import Solid.Types
import Solid.Exception
import Solid.Foreign.Haskell qualified as Haskell

import qualified Data.ByteString as B

import           ByteString ()
import           IO.Handle

readFile :: FilePath -> IO String
readFile file = do
  bytes <- readBinaryFile file
  case bytes.asString of
    Just string -> return string
    Nothing -> throwIO UnicodeDecodeError

writeFile :: FilePath -> String -> IO ()
writeFile = writeBinaryFile

readBinaryFile :: FilePath -> IO ByteString
readBinaryFile = Haskell.toFilePath >=> fmap Bytes . B.readFile

writeBinaryFile :: FilePath -> Bytes a -> IO ()
writeBinaryFile path (Bytes content) = Haskell.toFilePath path >>= (`B.writeFile` content)
