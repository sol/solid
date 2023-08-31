{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
module Solid.PP.Edit (
  Edit(..)
, insertClosingParen
, edit
, columnPragma
) where

import           Prelude ()
import           Solid.PP.IO
import           Solid.PP.SrcLoc (SrcLoc(..))
import           Data.List
import qualified Data.Text as T

data Edit = Replace {
  startColumn :: Maybe Int
, start :: Int
, old :: Int
, new :: Text
} deriving (Eq, Show)

insertClosingParen :: SrcLoc -> Edit
insertClosingParen loc = Replace Nothing loc.offset 0 (pragma <> ")")
  where
    pragma = mkColumnPragma (pred loc.column)

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
        forM_ (columnPragma step) putColumnPragma
        go (offset + n) xs

    put :: Text -> IO ()
    put = hPutStr h

    putColumnPragma :: Int -> IO ()
    putColumnPragma = put . mkColumnPragma

mkColumnPragma :: Int -> Text
mkColumnPragma (T.pack . show -> col) = "{-# COLUMN " <> col <> " #-}"

columnPragma :: Edit -> Maybe Int
columnPragma (Replace startColumn _ old (T.length -> new))
  | pragma == actual = Nothing
  | otherwise = pragma
  where
    pragma :: Maybe Int
    pragma = (fromIntegral old +) <$> startColumn

    actual :: Maybe Int
    actual = (fromIntegral new +) <$> startColumn
