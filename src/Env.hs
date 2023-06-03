{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
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
import Solid.Foreign.Haskell qualified as Haskell

import Data.Coerce (coerce)
import System.FilePath (searchPathSeparator)
import System.Directory.OsPath (findExecutable)

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
  getField PATH name = fmap Haskell.fromOsPath <$> findExecutable (Haskell.asOsPath name)

instance HasField "extend" PATH (FilePath -> IO a -> IO a) =>
         HasField "extend" PATH (FilePath -> IO a -> IO a) where
  getField PATH = extendPath

extendPath :: FilePath -> IO a -> IO a
extendPath (FilePath.asByteString -> dir) = bracket setup restore . const
  where
    setup :: IO (Maybe ByteString)
    setup = do
      old <- getPATH
      case old of
        Nothing -> setPATH dir
        Just p -> setPATH (dir <:> p)
      return old

    restore :: Maybe ByteString -> IO ()
    restore = maybe unsetPATH setPATH

    _PATH :: ByteString
    _PATH = "PATH"

    getPATH :: IO (Maybe ByteString)
    getPATH = Raw.get _PATH

    setPATH :: ByteString -> IO ()
    setPATH = Raw.set _PATH

    unsetPATH :: IO ()
    unsetPATH = Raw.unset _PATH

(<:>) :: ByteString -> ByteString -> ByteString
(<:>) dir p = dir <> [searchPathSeparator].pack.asByteString <> p
