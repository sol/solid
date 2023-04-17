{-# LANGUAGE UndecidableInstances #-}
module Env (
  get
, set
, unset

, ensure
, clear

, protect
, extend

, path
) where

import Solid

import Data.Coerce (coerce)
import System.FilePath (searchPathSeparator)
import System.Directory (findExecutable)

import ByteString qualified
import Env.Raw qualified as Raw
import Env.Raw (clear, protect)

get :: String -> IO (Maybe String)
get name = fmap ByteString.decodeUtf8 <$> Raw.get name.asByteString

set :: String -> String -> IO ()
set = coerce Raw.set

unset :: String -> IO ()
unset = coerce Raw.unset

ensure :: [(String, String)] -> IO ()
ensure = coerce Raw.ensure

extend :: [(String, String)] -> IO a -> IO a
extend = Raw.extend . coerce

path :: PATH
path = PATH

data PATH = PATH
  deriving Show

instance HasField "resolve" PATH (FilePath -> IO (Maybe FilePath)) where
  getField PATH name = fmap (.toFilePath) <$> findExecutable (unFilePath name)

instance HasField "extend" PATH (FilePath -> IO a -> IO a) => HasField "extend" PATH (FilePath -> IO a -> IO a) where
  getField PATH = extendPath

extendPath :: FilePath -> IO a -> IO a
extendPath dir action = do
  (setup, restore) <- getPATH <&> \ case
    Nothing -> (setPATH dir.toString, unsetPATH)
    Just p -> (setPATH $ dir <:> p, setPATH p)
  bracket_ setup restore action
  where
    _PATH = "PATH"
    getPATH = Env.get _PATH
    setPATH = Env.set _PATH
    unsetPATH = Env.unset _PATH

(<:>) :: FilePath -> String -> String
(<:>) (FilePath dir) p = pack dir <> pack [searchPathSeparator] <> p
