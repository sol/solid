{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
module Solid.PP.Edit (
  insert
, insert_
, replace
, replace_
, Edit(..)
, insertClosingParen
, edit
, columnPragma
) where

import           Prelude ()
import           Solid.PP.IO
import           Solid.PP.SrcLoc (BufferSpan(..), SrcLoc(..))
import           Data.List hiding (singleton, insert)
import           Solid.PP.DList
import qualified Data.Text as T

import           Solid.PP.Builder
import qualified Solid.PP.Builder as Builder

insert :: SrcLoc -> Text -> DList Edit
insert loc = singleton . Replace (Just loc.column) loc.offset 0

insert_ :: SrcLoc -> Text -> DList Edit
insert_ loc = singleton . Replace Nothing loc.offset 0

replace :: BufferSpan -> Text -> DList Edit
replace loc = singleton . Replace (Just loc.startColumn) loc.start loc.length

replace_ :: BufferSpan -> Text -> DList Edit
replace_ loc = singleton . Replace Nothing loc.start loc.length

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

edit :: Text -> [Edit] -> Builder
edit input = go 0 . sortOn (.start)
  where
    put = text

    go :: Int -> [Edit] -> Builder
    go cursor = \ case
      [] -> do
        put (T.drop cursor input)
      step@(Replace _ offset n substitute) : xs ->
           put (T.drop cursor $ T.take offset input)
        <> put substitute
        <> maybe mempty putColumnPragma (columnPragma step)
        <> go (offset + n) xs

    putColumnPragma :: Int -> Builder
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
