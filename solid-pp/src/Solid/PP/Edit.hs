{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
module Solid.PP.Edit (
  Edit(..)
, edit
, columnPragma
) where

import           Prelude ()
import           Solid.PP.IO
import           Data.List
import qualified Data.Text as T

newtype StartColumn = StartColumn Int
  deriving newtype (Eq, Show, Num, Ord)

data Edit = Replace {
  startColumn :: Maybe Int
, start :: Int
, old :: Int
, new :: Text
} deriving (Eq, Show)

edit :: Handle -> Text -> [Edit] -> IO ()
edit h input = go 0 . sortOn (.start)
  where
    go :: Int -> [Edit] -> IO ()
    go cursor = \ case
      [] -> do
        put (T.drop cursor input)
      step@(Replace _ offset n substitute) : xs -> do
        put (T.drop cursor $ T.take offset input)
        put substitute
        forM_ (columnPragma input step) putColumnPragma
        go (offset + n) xs

    put :: Text -> IO ()
    put = hPutStr h

    putColumnPragma :: StartColumn -> IO ()
    putColumnPragma (T.pack . show -> col) = put "{-# COLUMN " >> put col >> put " #-}"

columnPragma :: Text -> Edit -> Maybe StartColumn
columnPragma input (Replace startColumn offset old (T.length -> new))
  | pragma == actual = Nothing
  | workaroundForGhcIssue23040 = Nothing
  | otherwise = StartColumn <$> pragma
  where
    pragma = (fromIntegral old +) <$> startColumn
    actual = (fromIntegral new +) <$> startColumn

    workaroundForGhcIssue23040 :: Bool
    -- https://gitlab.haskell.org/ghc/ghc/-/issues/23040
    workaroundForGhcIssue23040 = "." `T.isPrefixOf` T.drop (offset + old) input
