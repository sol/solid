{-# LANGUAGE LambdaCase #-}
module Solid.PP.Edit where

import           Prelude ()
import           Solid.PP.IO

import qualified Data.Text as T

data Edit = Replace Int Char
  deriving (Eq, Show)

edit :: Handle -> Text -> [Edit] -> IO ()
edit h = go 0
  where
    go :: Int -> Text -> [Edit] -> IO ()
    go offset input = \ case
      [] -> do
        hPutStr h (T.drop offset input)
      Replace n c : xs -> do
        hPutStr h (T.drop offset $ T.take n input)
        hPutStr h (T.singleton c)
        go (succ n) input xs
