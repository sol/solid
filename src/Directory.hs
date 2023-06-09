module Directory (
  create
, ensure

, getCurrent
, setCurrent
, withCurrent
) where

import Solid
import Solid.Types

import Data.Coerce (coerce)
import System.Directory.OsPath qualified as Haskell

create :: FilePath -> IO ()
create = coerce Haskell.createDirectory

ensure :: FilePath -> IO ()
ensure = coerce $ Haskell.createDirectoryIfMissing True

getCurrent :: IO FilePath
getCurrent = coerce Haskell.getCurrentDirectory

setCurrent :: FilePath -> IO ()
setCurrent = coerce Haskell.setCurrentDirectory

withCurrent :: FilePath -> IO a -> IO a
withCurrent dir action = bracket getCurrent setCurrent $ \ _ -> do
  setCurrent dir
  action
