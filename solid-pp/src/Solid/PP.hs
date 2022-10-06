{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Solid.PP (run) where

import           Prelude ()
import           Solid.PP.IO

import           Control.Monad.Trans.State

import           Solid.PP.Lexer
import           Solid.PP.Edit

extensions :: [Extension]
extensions = [
    DataKinds
  , DeriveAnyClass
  , OverloadedRecordDot
  , OverloadedStrings
  ]

run :: FilePath -> FilePath -> FilePath -> IO ()
run src cur dst = do
  input <- readFile cur
  ppIdentifiers src dst $ addLinePragma (pp input)
  where
    addLinePragma = (linePragma <>)
    linePragma = "{-# LINE 1 " <> pack (show src) <> " #-}\n"

ppIdentifiers :: FilePath -> FilePath -> Text -> IO ()
ppIdentifiers src dst input = do
  tokens <- tokenize extensions src input
  withFile dst WriteMode $ \ h -> do
    edit h input (ppIdentifierSuffixes tokens)

allowedIdentifierSuffixes :: [(Char, Char)]
allowedIdentifierSuffixes = [
    ('!', 'ᴉ')
  , ('?', 'ʔ')
  ]

ppIdentifierSuffixes :: [PsLocated Token] -> [Edit]
ppIdentifierSuffixes = go
  where
    go = \ case
      [] -> []
      identifier@(L _ (ITvarid _)) : operator@(L loc (ITvarsym op)) : xs |
          notSeparatedByWhitespace identifier operator
        , suffix : rest <- unpackFS op
        , rest == "" || rest == "."
        , Just replacement <- lookup suffix allowedIdentifierSuffixes
            -> Replace (start loc) replacement : go xs
      _ : xs -> go xs

    notSeparatedByWhitespace :: PsLocated Token -> PsLocated Token -> Bool
    notSeparatedByWhitespace (L a _) (L b _) = end a == start b

    start :: PsSpan -> Int
    start = bufPos . bufSpanStart . psBufSpan

    end :: PsSpan -> Int
    end = bufPos . bufSpanEnd . psBufSpan

pp :: Text -> Text
pp = pack . go . unpack
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
