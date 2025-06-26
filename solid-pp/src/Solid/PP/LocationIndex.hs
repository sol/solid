{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Solid.PP.LocationIndex where

import Data.Text.Internal (Text(..))
import Data.Text qualified as Text
import Data.Maybe (fromMaybe)
import Control.Arrow ((>>>))
import Data.Text.Internal.Search qualified as Search
import Data.Map qualified as Map
import Data.Map (Map)

type Offset = Int
type Line = Int

type LocationIndex = Map Offset Entry

data Location = Location {
  line :: Int
, column :: Int
} deriving (Eq, Show)

data Entry = Entry {
  line :: Int
, text :: Text -- FIXME: only keep line if it has non-ASCII characters
} deriving (Eq, Show)

locationIndex :: Text -> LocationIndex
locationIndex t = Map.fromAscList $ zip indices foo
  where
    indices :: [Offset]
    indices = 0 : map succ (Search.indices "\n" t)

    foo :: [Entry]
    foo = zipWith Entry [1..] (Text.lines t)

offsetLocation :: Int -> LocationIndex -> Location
offsetLocation offset index = Location (offsetLine offset index) (offsetColumn offset index)

offsetLine :: Offset -> LocationIndex -> Line
offsetLine offset = fromMaybe maxBound . fmap (snd >>> (.line)) . Map.lookupLE offset

offsetColumn :: Offset -> LocationIndex -> Line
offsetColumn offset index = case Map.lookupLE offset index of
  Nothing -> offset + 1
  Just (start, entry) -> Text.length l + 1
    where
      l = case entry.text of
        Text arr off len -> Text arr off (min (offset - start) len)
