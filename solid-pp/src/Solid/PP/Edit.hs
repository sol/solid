{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
module Solid.PP.Edit (
  StartColumn(..)
, Edit(..)
, edit
, columnPragma
) where

import           Prelude ()
import           Solid.PP.IO

import           Solid.PP.SrcLoc (StartColumn(..))

import qualified Data.Text as T

data Edit = Replace (Maybe StartColumn) Int Int Text
  deriving (Eq, Show)

edit :: Handle -> Text -> [Edit] -> IO ()
edit h input = go 0
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

    putColumnPragma :: StartColumn -> IO ()
    putColumnPragma (T.pack . show -> col) = do
      put "{-# COLUMN " >> put col >> put " #-}"

columnPragma :: Edit -> Maybe StartColumn
columnPragma (Replace startColumn _ old (T.length -> new))
  | pragma == actual = Nothing
  | otherwise = pragma
  where
    pragma = (fromIntegral old +) <$> startColumn
    actual = (fromIntegral new +) <$> startColumn
