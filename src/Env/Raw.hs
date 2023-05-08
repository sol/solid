{-# LANGUAGE CPP #-}
module Env.Raw (
  get
, set
, unset

, all
, ensure
, clear

, protect
, extend
, modify
) where

import Solid hiding (all)
import Data.Coerce (coerce)
import System.Posix.Env (getEnvironmentPrim)
import System.Posix.Env.ByteString qualified as Haskell

get :: ByteString -> IO (Maybe ByteString)
get = coerce Haskell.getEnv

set :: ByteString -> ByteString -> IO ()
set name value = Haskell.setEnv (unBytes name) (unBytes value) True

unset :: ByteString -> IO ()
unset = coerce Haskell.unsetEnv

all :: IO [(ByteString, ByteString)]
#if MIN_VERSION_unix(2,8,2)
all = coerce Haskell.getEnvironment
#else
all = getEnvironmentPrim >>= \ case
  [] -> return []
  _ -> coerce Haskell.getEnvironment
#endif

ensure :: [(ByteString, ByteString)] -> IO ()
ensure = Haskell.setEnvironment . reverse . coerce

clear :: IO ()
clear = Haskell.clearEnv

protect :: IO a -> IO a
protect action = bracket Haskell.getEnvironment Haskell.setEnvironment $ \ _ -> action

extend :: [(ByteString, ByteString)] -> IO a -> IO a
extend env = modify (env ++)

modify :: ([(ByteString, ByteString)] -> [(ByteString, ByteString)]) -> IO a -> IO a
modify f action = bracket Haskell.getEnvironment Haskell.setEnvironment $ \ env -> do
  ensure (f $ coerce env)
  action