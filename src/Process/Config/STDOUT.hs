{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Process.Config.STDOUT (
  inherit
, null
, capture
, useFile
, createPipe
, useHandle
, useAndCloseHandle
, STDOUT(..)
) where

import Solid hiding (null)
import Solid.Bytes.Unsafe

import Control.Concurrent.STM qualified as STM
import Data.ByteString.Lazy qualified as LB
import Solid.Process.Typed (Config)
import Solid.Process.Typed qualified as Haskell

import Process.Config.FileStream

inherit :: Config stdin stdout stderr -> Config stdin () stderr
inherit = Haskell.setStdout Haskell.inherit

null :: Config stdin stdout stderr -> Config stdin () stderr
null = Haskell.setStdout Haskell.nullStream

capture :: Config stdin stdout stderr -> Config stdin (IO ByteString) stderr
capture = Haskell.setStdout (STM.atomically <$> fmap (Bytes . LB.toStrict) <$> Haskell.byteStringOutput)

useFile :: FilePath -> Config stdin stdout stderr -> Config stdin () stderr
useFile = Haskell.setStdout . fileOutput

createPipe :: Config stdin stdout stderr -> Config stdin Handle stderr
createPipe = Haskell.setStdout Haskell.createPipe

useHandle :: Handle -> Config stdin stdout stderr -> Config stdin () stderr
useHandle = Haskell.setStdout . Haskell.useHandleOpen

useAndCloseHandle :: Handle -> Config stdin stdout stderr -> Config stdin () stderr
useAndCloseHandle = Haskell.setStdout . Haskell.useHandleClose

newtype STDOUT stdin stdout stderr = STDOUT (Config stdin stdout stderr)

instance HasField "inherit" (STDOUT stdin stdout stderr) (Config stdin () stderr) where
  getField (STDOUT config) = inherit config

instance HasField "null" (STDOUT stdin stdout stderr) (Config stdin () stderr) where
  getField (STDOUT config) = null config

instance HasField "capture" (STDOUT stdin stdout stderr) (Config stdin (IO ByteString) stderr) where
  getField (STDOUT config) = capture config

instance HasField "useFile" (STDOUT stdin stdout stderr) (FilePath -> Config stdin () stderr) where
  getField (STDOUT config) file = useFile file config

instance HasField "createPipe" (STDOUT stdin stdout stderr) (Config stdin Handle stderr) where
  getField (STDOUT config) = createPipe config

instance HasField "useHandle" (STDOUT stdin stdout stderr) (Handle -> Config stdin () stderr) where
  getField (STDOUT config) = flip useHandle config

instance HasField "useAndCloseHandle" (STDOUT stdin stdout stderr) (Handle -> Config stdin () stderr) where
  getField (STDOUT config) = flip useAndCloseHandle config
