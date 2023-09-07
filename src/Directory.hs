{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Directory (
  create
, ensure

, getCurrent
, setCurrent
, withCurrent
) where

import Solid
use System.Directory.Import

create :: FilePath -> IO ()
create = Import.createDirectory

ensure :: FilePath -> IO ()
ensure = Import.createDirectoryIfMissing True

getCurrent :: IO FilePath
getCurrent = Import.getCurrentDirectory

setCurrent :: FilePath -> IO ()
setCurrent = Import.setCurrentDirectory

withCurrent :: FilePath -> IO a -> IO a
withCurrent dir action = bracket getCurrent setCurrent $ \ _ -> do
  setCurrent dir
  action
