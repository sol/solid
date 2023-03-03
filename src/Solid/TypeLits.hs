{-# LANGUAGE AllowAmbiguousTypes #-}
module Solid.TypeLits (
  Symbol
, KnownSymbol
, symbolValue
) where

import Solid

import GHC.TypeLits
import Data.Proxy

symbolValue :: forall symbol. KnownSymbol symbol => String
symbolValue = pack $ symbolVal @symbol Proxy
