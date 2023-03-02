{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Solid.String (
  Bytes(..)
, String
, ByteString
, pack
, unpack
, lines
, unlines
) where

import           Solid.Common
import           Solid.Exception
import           Solid.ByteString
import           Solid.String.Type

import qualified Data.ByteString as Haskell

asString! :: HasCallStack => Bytes a -> String
asString! (Bytes string) = if Haskell.isValidUtf8 string then Bytes string else throw! UnicodeDecodeError

instance HasField "asString\7433" (Bytes a) String where
  getField = asString!
