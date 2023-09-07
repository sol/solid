{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Exception (
  Exception
, fromException
, SomeException
, StackTrace

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
, finally

, InvalidValue(..)
, invalidValue!
) where

import Solid.Common
import Solid.String
import Solid.Bytes.Unsafe (FilePath)
import Solid.Bytes (asFilePath)
import Solid.ToString as ToString
import Solid.StackTrace (StackTrace)
import Solid.StackTrace qualified as StackTrace

import           Control.Exception (Exception(toException), SomeException, evaluate, throwIO, bracket, bracket_, finally)

import qualified HaskellPrelude as Haskell
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

error! :: WithStackTrace => String -> a
error! message = StackTrace.suppress $ Haskell.error (unpack message)

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
    (NoSuchThing, "No such file or directory", Just name) -> FileNotFoundError (toFilePath name)
    (InappropriateType, "is a directory", Just name) -> IsADirectoryError (toFilePath name)
    (PermissionDenied, "Permission denied", Just name) -> PermissionError (toFilePath name)
    _ -> ForeignIOError err
    where
      toFilePath :: [Char] -> FilePath
      toFilePath = asFilePath . pack

data InvalidValue = InvalidValue StackTrace String
  deriving Eq

instance Show InvalidValue where
  show = unpack . toString

instance Exception InvalidValue

instance ToString InvalidValue where
  toString (InvalidValue trace message) = "{if trace.empty? then "" else "\n\n{trace}\n\n"}InvalidValue: {message}"

instance HasField "toString" InvalidValue String where
  getField = toString

invalidValue! :: WithStackTrace => String -> a
invalidValue! = throw! . InvalidValue StackTrace.retrieve
