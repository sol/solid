{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Process.Config (
  Config
, stdin
, stdout
, stderr
, environment
, chdir
) where

import Solid hiding (stdin, stdout, stderr)

import Solid.Process.Typed (Config)
import Solid.Process.Typed qualified as Haskell

import Haskell qualified

import Process.Config.STDIN
import Process.Config.STDOUT
import Process.Config.STDERR

.stdin :: Config stdin stdout stderr -> STDIN stdin stdout stderr
.stdin = STDIN

.stdout :: Config stdin stdout stderr -> STDOUT stdin stdout stderr
.stdout = STDOUT

.stderr :: Config stdin stdout stderr -> STDERR stdin stdout stderr
.stderr = STDERR

.environment :: [(String, String)] -> Config stdin stdout stderr -> Config stdin stdout stderr
.environment = Haskell.setEnv . map (bimap unpack unpack)

.chdir :: FilePath -> Config stdin stdout stderr -> Config stdin stdout stderr
.chdir = Haskell.setWorkingDir . Haskell.toFilePath!
