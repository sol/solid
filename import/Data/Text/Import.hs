{-# OPTIONS_GHC -F -pgmF solid-pp #-}
module Data.Text.Import (
  String.pack
, String.unpack
, String.strip
, module Data.Text.Import
) where

import Prelude
import Haskell (fromText, toText)
use Data.Text

type Text = String

unwords :: [Text] -> Text
unwords = fromText . Text.unwords . map toText

toTitle :: Text -> Text
toTitle = fromText . Text.toTitle . toText

toLower :: Text -> Text
toLower = fromText . Text.toLower . toText

span :: (Char -> Bool) -> Text -> (Text, Text)
span p = bimap fromText fromText . Text.span p . toText

break :: (Char -> Bool) -> Text -> (Text, Text)
break p = bimap fromText fromText . Text.break p . toText
