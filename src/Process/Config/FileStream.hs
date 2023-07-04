{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Process.Config.FileStream where

import Solid

import System.Process (StdStream(..))
import System.Process.Typed qualified as Haskell
import System.Process.Typed.Internal qualified as Haskell

fileStream :: FilePath -> IO.Mode -> Haskell.StreamSpec anyStreamType ()
fileStream name mode = Haskell.mkManagedStreamSpec acquire release
  where
    acquire :: (StdStream -> IO a) -> IO a
    acquire action = with (IO.open name mode) $ \ handle -> action (UseHandle handle)

    release :: Haskell.ProcessConfig () () () -> Maybe Handle -> IO ((), IO ())
    release _ _ = pure ((), return ())

fileInput :: FilePath -> Haskell.StreamSpec 'Haskell.STInput ()
fileInput name = fileStream name IO.ReadMode

fileOutput :: FilePath -> Haskell.StreamSpec 'Haskell.STOutput ()
fileOutput name = fileStream name IO.WriteMode
