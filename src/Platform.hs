{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# LANGUAGE CPP #-}
module Platform where

import Solid

windows? :: Bool
windows? =
#ifdef mingw32_HOST_OS
  True
#else
  False
#endif
