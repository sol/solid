{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.Exception (
  HasCallStack

, Exception
, fromException
, SomeException

, IOException(..)
, UnicodeDecodeError(..)

, throw!
, error!
, throwIO
, bracket
, bracket_

, evaluate
, try
, catch
, handle
) where

import           Solid.Common
import           Solid.String.Type
import           Solid.FilePath

import           GHC.Stack
import           Control.Exception (Exception(toException), SomeException, evaluate, throwIO, bracket, bracket_)

import qualified Prelude as Haskell
import qualified Control.Exception as Haskell

import           GHC.IO.Exception hiding (IOException)

data IOException =
    FileNotFoundError FilePath
  | IsADirectoryError FilePath
  | PermissionError FilePath
  | ForeignIOError IOError
  deriving (Eq, Show, Exception)

data UnicodeDecodeError = UnicodeDecodeError
  deriving (Eq, Show, Exception)

throw! :: Exception e => e -> a
throw! = Haskell.throw

error! :: HasCallStack => String -> a
error! message = withFrozenCallStack $ Haskell.error (unpack message)

try :: Exception e => IO a -> IO (Either e a)
try a = catch (a >>= \ v -> return (Right v)) (\e -> return (Left e))

catch :: Exception e => IO a -> (e -> IO a) -> IO a
catch action handler = Haskell.catch action $ \ err -> case fromException err of
  Just e -> handler e
  Nothing -> throwIO err

handle :: Exception e => (e -> IO a) -> IO a -> IO a
handle = flip catch

fromException :: Exception e => SomeException -> Maybe e
fromException = Haskell.fromException . transformException

transformException :: SomeException -> SomeException
transformException e = case Haskell.fromException e of
  Nothing -> e
  Just err -> toException $ case (ioe_type err, ioe_description err, ioe_filename err) of
    (NoSuchThing, "No such file or directory", Just name) -> FileNotFoundError (FilePath name)
    (InappropriateType, "is a directory", Just name) -> IsADirectoryError (FilePath name)
    (PermissionDenied, "Permission denied", Just name) -> PermissionError (FilePath name)
    _ -> ForeignIOError err
