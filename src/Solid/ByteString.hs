{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
module Solid.ByteString where

import Solid.Common
import Solid.Bytes.Unsafe
import GHC.Exts

type ByteString = Bytes Word8

deriving newtype instance Semigroup ByteString
deriving newtype instance Monoid ByteString
deriving newtype instance IsList ByteString
