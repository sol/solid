{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Env.Raw (
  get
, set
, unset

, without

, all
, ensure
, clear

, protect
, extend
, modify
) where

import Solid hiding (all)
import System.Posix.Env.ByteString qualified as Haskell

use Haskell

get :: ByteString -> IO (Maybe ByteString)
get = fmap (fmap Haskell.fromByteString) . Haskell.getEnv . Haskell.toByteString

set :: ByteString -> ByteString -> IO ()
set name value = Haskell.setEnv (Haskell.toByteString name) (Haskell.toByteString value) True

unset :: ByteString -> IO ()
unset = Haskell.unsetEnv . Haskell.toByteString

without :: ByteString -> IO a -> IO a
without name action = bracket (get name) (maybe pass (set name)) $ \ _ -> do
  unset name
  action

all :: IO [(ByteString, ByteString)]
all = fromHaskellEnvironment <$> Haskell.getEnvironment

ensure :: [(ByteString, ByteString)] -> IO ()
ensure = Haskell.setEnvironment . reverse . toHaskellEnvironment

clear :: IO ()
clear = Haskell.clearEnv

protect :: IO a -> IO a
protect action = bracket Haskell.getEnvironment Haskell.setEnvironment $ \ _ -> action

extend :: [(ByteString, ByteString)] -> IO a -> IO a
extend values = modify (values ++)

modify :: ([(ByteString, ByteString)] -> [(ByteString, ByteString)]) -> IO a -> IO a
modify f action = bracket Haskell.getEnvironment Haskell.setEnvironment $ \ env -> do
  ensure (f $ fromHaskellEnvironment env)
  action

toHaskellEnvironment :: [(ByteString, ByteString)] -> [(Haskell.ByteString, Haskell.ByteString)]
toHaskellEnvironment = map (bimap Haskell.toByteString Haskell.toByteString)

fromHaskellEnvironment :: [(Haskell.ByteString, Haskell.ByteString)] -> [(ByteString, ByteString)]
fromHaskellEnvironment = map (bimap Haskell.fromByteString Haskell.fromByteString)
