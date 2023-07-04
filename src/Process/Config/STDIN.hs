{-# LANGUAGE UndecidableInstances #-}
module Process.Config.STDIN (
  inherit
, null
, set
, setBytes
, setByteString
, useFile
, createPipe
, useHandle
, useAndCloseHandle
, STDIN(..)
) where

import Solid hiding (null)
import Solid.Types

import Data.ByteString.Lazy qualified as LB
import System.Process.Typed (Config)
import System.Process.Typed qualified as Haskell

import Process.Config.FileStream

inherit :: Config stdin stdout stderr -> Config () stdout stderr
inherit = Haskell.setStdin Haskell.inherit

null :: Config stdin stdout stderr -> Config () stdout stderr
null = Haskell.setStdin Haskell.nullStream

set :: String -> Config stdin stdout stderr -> Config () stdout stderr
set = setBytes

setBytes :: Bytes a -> Config stdin stdout stderr -> Config () stdout stderr
setBytes input = setByteString input.asByteString

setByteString :: ByteString -> Config stdin stdout stderr -> Config () stdout stderr
setByteString = Haskell.setStdin . Haskell.byteStringInput . LB.fromStrict . unBytes

useFile :: FilePath -> Config stdin stdout stderr -> Config () stdout stderr
useFile = Haskell.setStdin . fileInput

createPipe :: Config stdin stdout stderr -> Config Handle stdout stderr
createPipe = Haskell.setStdin Haskell.createPipe

useHandle :: Handle -> Config stdin stdout stderr -> Config () stdout stderr
useHandle = Haskell.setStdin . Haskell.useHandleOpen

useAndCloseHandle :: Handle -> Config stdin stdout stderr -> Config () stdout stderr
useAndCloseHandle = Haskell.setStdin . Haskell.useHandleClose

newtype STDIN stdin stdout stderr = STDIN (Config stdin stdout stderr)

instance HasField "inherit" (STDIN stdin stdout stderr) (Config () stdout stderr) where
  getField (STDIN config) = inherit config

instance HasField "null" (STDIN stdin stdout stderr) (Config () stdout stderr) where
  getField (STDIN config) = null config

instance HasField "set" (STDIN stdin stdout stderr) (String -> Config () stdout stderr) where
  getField (STDIN config) = flip set config

instance HasField "setBytes" (STDIN stdin stdout stderr) (Bytes a -> Config () stdout stderr) =>
         HasField "setBytes" (STDIN stdin stdout stderr) (Bytes a -> Config () stdout stderr) where
  getField (STDIN config) = flip setBytes config

instance HasField "setByteString" (STDIN stdin stdout stderr) (ByteString -> Config () stdout stderr) where
  getField (STDIN config) = flip setByteString config

instance HasField "useFile" (STDIN stdin stdout stderr) (FilePath -> Config () stdout stderr) where
  getField (STDIN config) file = useFile file config

instance HasField "createPipe" (STDIN stdin stdout stderr) (Config Handle stdout stderr) where
  getField (STDIN config) = createPipe config

instance HasField "useHandle" (STDIN stdin stdout stderr) (Handle -> Config () stdout stderr) where
  getField (STDIN config) = flip useHandle config

instance HasField "useAndCloseHandle" (STDIN stdin stdout stderr) (Handle -> Config () stdout stderr) where
  getField (STDIN config) = flip useAndCloseHandle config
