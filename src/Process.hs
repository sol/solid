{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE UndecidableInstances #-}
module Process (
  Config
, CmdSpec(..)
, Process
, ExitCode(..)
, ExitStatusException(..)

, raw
, command
, shell

, spawn
, terminate

, wait
, status
, checkStatus
, stdin
, stdout
, stderr

, Process.with
, run
, read

, escape

, Self.name
, Self.exit
, Self.args
) where

import Solid hiding (stdin, stdout, stderr, read)

import Control.Concurrent.MVar
import System.Exit (ExitCode(..))

import System.Process (CmdSpec(..))
import System.Process.Internals (translate)
import Solid.Process.Typed qualified as Haskell
import Solid.Process.Typed.Internal qualified as Haskell

import Solid.Foreign.Haskell qualified as Haskell
import Process.Config (Config)
import Process.Self qualified as Self

escape :: String -> String
escape = pack . translate . unpack

instance ToString CmdSpec where
  toString = \ case
    ShellCommand cmd -> pack cmd
    RawCommand cmd args -> unwords . map (pack . translate) $ cmd : args

data CheckStatusOnWait = CheckStatusOnWait | DontCheckStatusOnWait

data Process stdin stdout stderr = Process {
  handle :: Haskell.Process stdin stdout stderr
, checkStatusOnWait :: MVar CheckStatusOnWait
}

data ExitStatusException = ExitStatusException {
  status :: Int
, command :: CmdSpec
} deriving (Eq, Show, Exception)

instance ToString ExitStatusException where
  toString err = "Command `{err.command}` returned non-zero exit status {err.status}."

throwExitStatusException :: Process stdin stdout stderr -> Int -> IO ()
throwExitStatusException process st = throwIO ExitStatusException {
  status = st
, command = Haskell.pcCmdSpec $ Haskell.pConfig process.handle
}

raw :: FilePath -> [ByteString] -> Config () () ()
raw cmd = Haskell.setDelegateCtlc True . Haskell.proc (Haskell.toFilePath! cmd) . map encodeArg
  where
    encodeArg :: ByteString -> [Char]
    encodeArg = Haskell.toFilePath! . ByteString.asFilePath

command :: FilePath -> [String] -> Config () () ()
command cmd = Haskell.setDelegateCtlc True . Haskell.proc (Haskell.toFilePath! cmd) . map unpack

shell :: String -> Config () () ()
shell = Haskell.setDelegateCtlc True . Haskell.shell . unpack

spawn :: Config stdin stdout stderr -> IO (Process stdin stdout stderr)
spawn config = Process <$> Haskell.startProcess config <*> newMVar CheckStatusOnWait

terminate :: Process stdin stdout stderr -> IO ()
terminate = Haskell.stopProcess . (.handle)

instance HasField "spawn" (Config stdin stdout stderr) (IO (Process stdin stdout stderr)) where
  getField = spawn

instance HasField "terminate" (Process stdin stdout stderr) (IO ()) where
  getField = terminate

wait :: Process stdin stdout stderr -> IO ()
wait process = withMVar process.checkStatusOnWait $ \ check -> do
  Haskell.waitExitCode process.handle >>= \ case
    ExitSuccess -> pass
    ExitFailure code -> case check of
      CheckStatusOnWait -> throwExitStatusException process code
      DontCheckStatusOnWait -> pass

status :: Process stdin stdout stderr -> IO ExitCode
status process = modifyMVar process.checkStatusOnWait $ \ _ -> do
  st <- Haskell.waitExitCode process.handle
  return (DontCheckStatusOnWait, st)

checkStatus :: Process stdin stdout stderr -> IO ()
checkStatus process = status process >>= \ case
  ExitSuccess -> pass
  ExitFailure code -> throwExitStatusException process code

stdin :: Process stdin stdout stderr -> stdin
stdin = Haskell.getStdin . (.handle)

stdout :: Process stdin stdout stderr -> stdout
stdout = Haskell.getStdout . (.handle)

stderr :: Process stdin stdout stderr -> stderr
stderr = Haskell.getStderr . (.handle)

instance HasField "wait" (Process stdin stdout stderr) (IO ()) where
  getField = wait

instance HasField "status" (Process stdin stdout stderr) (IO ExitCode) where
  getField = status

instance HasField "checkStatus" (Process stdin stdout stderr) (IO ()) where
  getField = checkStatus

instance HasField "stdin" (Process stdin stdout stderr) stdin where
  getField = stdin

instance HasField "stdout" (Process stdin stdout stderr) stdout where
  getField = stdout

instance HasField "stderr" (Process stdin stdout stderr) stderr where
  getField = stderr

with :: Config stdin stdout stderr -> (Process stdin stdout stderr -> IO a) -> IO a
with config action = bracket config.spawn terminate $ \ process -> do
  action process <* wait process

run :: Config stdin stdout stderr -> IO ()
run config = Process.with config checkStatus

read :: Config () () () -> IO ByteString
read config = Process.with config.stdout.capture stdout

instance HasField "with" (Config stdin stdout stderr) ((Process stdin stdout stderr -> IO a) -> IO a)
      => HasField "with" (Config stdin stdout stderr) ((Process stdin stdout stderr -> IO a) -> IO a) where
  getField = Process.with

instance HasField "run" (Config () () ()) (IO ()) where
  getField = run

instance HasField "read" (Config () () ()) (IO ByteString) where
  getField = read
