{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE PackageImports #-}
module Solid.OsString (
  OsString(..)
, PosixString(..)
) where

import "filepath" System.OsString.Internal.Types (OsString(..), PosixString(..))
