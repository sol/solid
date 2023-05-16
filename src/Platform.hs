{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Platform where

import Solid

import System.Info (os)

windows? :: Bool
windows? = os == "mingw32"
