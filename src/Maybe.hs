{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Maybe (
  nothing?
, just?
) where

import Solid.Common
import Data.Maybe

nothing? :: Maybe a -> Bool
nothing? = isNothing

just? :: Maybe a -> Bool
just? = isJust

instance HasField "nothing\660" (Maybe a) Bool where
  getField = nothing?

instance HasField "just\660" (Maybe a) Bool where
  getField = just?
