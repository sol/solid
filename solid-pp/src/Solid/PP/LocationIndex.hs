{-# LANGUAGE OverloadedStrings #-}
module Solid.PP.LocationIndex where

import Data.Maybe (fromMaybe)
import Data.Text.Internal.Search qualified as Search
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Map (Map)

type Offset = Int
type Line = Int

type LocationIndex = Map Offset Line

locationIndex :: Text -> LocationIndex
locationIndex t = Map.fromAscList $ zip (map succ $ Search.indices "\n" t) [2..]

offsetLine :: Offset -> LocationIndex -> Line
offsetLine offset = fromMaybe 1 . fmap snd . Map.lookupLE offset
