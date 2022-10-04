{-# LANGUAGE LambdaCase #-}
module Solid.PP where

import           Control.Monad.Trans.State

run :: FilePath -> FilePath -> FilePath -> IO ()
run src cur dst = readFile cur >>= writeFile dst . (linePragma <>) . pp
  where
    linePragma = "{-# LINE 1 " <> show src <> " #-}\n"

pp :: String -> String
pp = go
  where
    go :: String -> String
    go = \ case
      '"' : xs -> case runState (stringLiteral xs) False of
        ((rest, lit), True) -> '(' : '"' : lit ++ "\")" ++ go rest
        ((rest, lit), False) -> '"' : lit ++ "\"" ++ go rest
      x : xs -> x : go xs
      [] -> []

    stringLiteral :: String -> State Bool (String, String)
    stringLiteral = \ case
      '"' : xs -> return (xs, "")
      '\\' : '{' : xs -> fmap ('{' :) <$> stringLiteral xs
      '{' : xs -> fmap ("\" <> toString (" <>) <$> interpolation xs
      x : xs -> fmap (x :) <$> stringLiteral xs
      [] -> return ("", "")

    interpolation :: String -> State Bool (String, String)
    interpolation input = do
      put True
      case input of
        '}' : xs -> fmap (") <> \"" <>) <$> stringLiteral xs
        x : xs -> fmap (x :) <$> interpolation xs
        [] -> return ("", "")
