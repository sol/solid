{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Solid.PP (run, extensions) where

import           Prelude ()
import           Solid.PP.IO

import           Data.Char
import           Data.Word
import qualified Data.Text as T
import qualified Data.ByteString.Short as SB
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

desugarIdentifier :: Int -> Int -> FastString -> [Edit] -> [Edit]
desugarIdentifier start end identifier
  | lastChar == qmark || lastChar == bang = (Replace start (end - start) replacement :)
  | otherwise = id
  where
    lastChar :: Word8
    lastChar = SB.last $ fastStringToShortByteString identifier

    qmark :: Word8
    qmark = fromIntegral $ ord '?'

    bang :: Word8
    bang = fromIntegral $ ord '!'

    replacement :: Text
    replacement = T.pack $ map replaceChar $ unpackFS identifier

    replaceChar :: Char -> Char
    replaceChar c = case c of
      '!' -> 'ᴉ'
      '?' -> 'ʔ'
      _ -> c

ppIdentifierSuffixes :: [PsLocated Token] -> [Edit]
ppIdentifierSuffixes = go
  where
    go = \ case
      [] -> []
      L loc (ITvarid identifier) : xs -> desugarIdentifier (start loc) (end loc) identifier $ go xs
      L loc (ITqvarid (succ . lengthFS -> offset, identifier)) : xs -> desugarIdentifier (start loc + offset) (end loc) identifier $ go xs
      _ : xs -> go xs

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
