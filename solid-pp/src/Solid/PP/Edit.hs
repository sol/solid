{-# LANGUAGE LambdaCase #-}
module Solid.PP.Edit where

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
        hPutStr h (T.drop cursor input)
      Replace pos n substitute : xs -> do
        hPutStr h (T.drop cursor $ T.take pos input)
        hPutStr h substitute
        go (pos + n) xs
