{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE NoDeriveAnyClass #-}

-- | Mostly for compatibility across different base Prelude changes.
--
module Hedgehog.Internal.Prelude (
    Semigroup(..)
  , MonadFail
  , module Prelude
  ) where

import           Control.Monad.Fail (MonadFail)

import           Data.Semigroup (Semigroup(..))

import           HaskellPrelude as Prelude hiding (filter, print, map)
