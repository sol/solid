{-# LANGUAGE LambdaCase #-}
module Solid.PP where

run :: FilePath -> FilePath -> FilePath -> IO ()
run src cur dst = readFile cur >>= writeFile dst . (linePragma <>) . pp
  where
    linePragma = "{-# LINE 1 " <> show src <> " #-}\n"

pp :: String -> String
pp = go
  where
    go = \ case
      '"' : xs -> '(' : '"' : stringLiteral xs
      x : xs -> x : go xs
      [] -> []

    stringLiteral = \ case
      '"' : xs -> '"' : ')' : go xs
      '\\' : '{' : xs -> '{' : stringLiteral xs
      '{' : xs -> "\" <> toString (" <> interpolation xs
      x : xs -> x : stringLiteral xs
      [] -> []

    interpolation = \ case
      '}' : xs -> ") <> \"" <> stringLiteral xs
      x : xs -> x : interpolation xs
      [] -> []
