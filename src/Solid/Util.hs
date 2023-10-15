{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.Util (createAtomic) where

import Solid

import System.IO.Error
import GHC.IO.Exception
import Control.Exception (tryJust)

createAtomic :: FilePath -> (FilePath -> IO a) -> IO a
createAtomic dst action = do
  Directory.ensure dst.parent
  Temp.withDirectoryAt dst.parent $ \ tmp -> do
    a <- action tmp
    _ <- tryJust (guard . isUnsatisfiedConstraintsError) $ tmp.rename dst
    return a

isUnsatisfiedConstraintsError :: IOError -> Bool
isUnsatisfiedConstraintsError  = (== UnsatisfiedConstraints) . ioeGetErrorType
