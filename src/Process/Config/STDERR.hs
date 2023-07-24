{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Process.Config.STDERR (
  inherit
, null
, capture
, toStdout
, useFile
, createPipe
, useHandle
, useAndCloseHandle
, STDERR(..)
) where

import Solid hiding (null)
import Solid.Types

import Control.Concurrent.STM qualified as STM
import Data.ByteString.Lazy qualified as LB
import Solid.Process.Typed (Config)
import Solid.Process.Typed qualified as Haskell

import Process.Config.FileStream

inherit :: Config stdin stdout stderr -> Config stdin stdout ()
inherit = Haskell.setStderr Haskell.inherit

null :: Config stdin stdout stderr -> Config stdin stdout ()
null = Haskell.setStderr Haskell.nullStream

capture :: Config stdin stdout stderr -> Config stdin stdout (IO ByteString)
capture = Haskell.setStderr (STM.atomically <$> fmap (Bytes . LB.toStrict) <$> Haskell.byteStringOutput)

toStdout :: Config stdin stdout stderr -> Config stdin stdout ()
toStdout = useHandle stdout

useFile :: FilePath -> Config stdin stdout stderr -> Config stdin stdout ()
useFile = Haskell.setStderr . fileOutput

createPipe :: Config stdin stdout stderr -> Config stdin stdout Handle
createPipe = Haskell.setStderr Haskell.createPipe

useHandle :: Handle -> Config stdin stdout stderr -> Config stdin stdout ()
useHandle = Haskell.setStderr . Haskell.useHandleOpen

useAndCloseHandle :: Handle -> Config stdin stdout stderr -> Config stdin stdout ()
useAndCloseHandle = Haskell.setStderr . Haskell.useHandleClose

newtype STDERR stdin stdout stderr = STDERR (Config stdin stdout stderr)

instance HasField "inherit" (STDERR stdin stdout stderr) (Config stdin stdout ()) where
  getField (STDERR config) = inherit config

instance HasField "null" (STDERR stdin stdout stderr) (Config stdin stdout ()) where
  getField (STDERR config) = null config

instance HasField "capture" (STDERR stdin stdout stderr) (Config stdin stdout (IO ByteString)) where
  getField (STDERR config) = capture config

instance HasField "toStdout" (STDERR stdin stdout stderr) (Config stdin stdout ()) where
  getField (STDERR config) = toStdout config

instance HasField "useFile" (STDERR stdin stdout stderr) (FilePath -> Config stdin stdout ()) where
  getField (STDERR config) file = useFile file config

instance HasField "createPipe" (STDERR stdin stdout stderr) (Config stdin stdout Handle) where
  getField (STDERR config) = createPipe config

instance HasField "useHandle" (STDERR stdin stdout stderr) (Handle -> Config stdin stdout ()) where
  getField (STDERR config) = flip useHandle config

instance HasField "useAndCloseHandle" (STDERR stdin stdout stderr) (Handle -> Config stdin stdout ()) where
  getField (STDERR config) = flip useAndCloseHandle config
