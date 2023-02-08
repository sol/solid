{-# LANGUAGE LambdaCase #-}
module Solid.PP.Edit (
  Edit(..)
, edit
) where

import           Prelude ()
import           Solid.PP.IO

import qualified Data.Text as T

data Edit = Replace Int Int Text
  deriving (Eq, Show)

edit :: Handle -> Text -> [Edit] -> IO ()
edit h input = go 0
  where
    go :: Int -> [Edit] -> IO ()
    go cursor = \ case
      [] -> do
        put (T.drop cursor input)
      Replace offset n substitute : xs -> do
        put (T.drop cursor $ T.take offset input)
        put substitute
        go (offset + n) xs

    put :: Text -> IO ()
    put = hPutStr h
