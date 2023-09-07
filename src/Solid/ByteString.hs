{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
module Solid.ByteString where

import Solid.Common
import Solid.Bytes.Unsafe

type ByteString = Bytes Word8

deriving newtype instance Semigroup ByteString
deriving newtype instance Monoid ByteString
