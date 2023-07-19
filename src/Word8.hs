{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Word8 (
  Word8
, read
, read!
) where

import Solid.Common hiding (read)
import Solid.StackTrace qualified as StackTrace

read :: String.String -> Maybe Word8
read = String.read

read! :: WithStackTrace => String.String -> Word8
read! = StackTrace.suppress String.read!
