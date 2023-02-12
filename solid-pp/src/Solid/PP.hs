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
  ppIdentifiers src dst $ addLinePragma input
  where
    addLinePragma = (linePragma <>)
    linePragma = "{-# LINE 1 " <> pack (show src) <> " #-}\n"

ppIdentifiers :: FilePath -> FilePath -> Text -> IO ()
ppIdentifiers src dst input = do
  tokens <- tokenize extensions src input
  withFile dst WriteMode $ \ h -> do
    edit h input (pp tokens)

desugarIdentifier :: Int -> Int -> FastString -> [Edit] -> [Edit]
desugarIdentifier start end identifier
  | lastChar == qmark || lastChar == bang = (replace start end replacement :)
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

replace :: Int -> Int -> Text -> Edit
replace start end = Replace Nothing start (end - start)

unescapeString :: String -> String
unescapeString = go
  where
    go = \ case
      [] -> []
      '\\' : '{' : xs -> '{' : go xs
      x : xs -> x : go xs

unescapeStringLiteral :: BufferSpan -> String -> [Edit] -> [Edit]
unescapeStringLiteral loc old
  | new == old = id
  | otherwise = (Replace (Just loc.startColumn) loc.start (length old) (pack new) :)
  where
    new = unescapeString old

pp :: [WithBufferSpan Token] -> [Edit]
pp = go
  where
    go = \ case
      [] -> []
      L loc (ITvarid identifier) : xs -> desugarIdentifier (loc.start) (loc.end) identifier $ go xs
      L loc (ITqvarid (succ . lengthFS -> offset, identifier)) : xs -> desugarIdentifier (loc.start + offset) (loc.end) identifier $ go xs

      L loc (ITstring (SourceText src) _) : xs -> unescapeStringLiteral loc src $ go xs

      L loc (ITstring_interpolation_end_begin (SourceText src) _) : xs -> replaceStringSegment loc src (endInterpolation . beginInterpolation) : go xs
      L loc (ITstring_interpolation_end (SourceText src) _)       : xs -> replaceStringSegment loc src (endInterpolation . (<> ")")) : go xs
      L loc (ITstring_interpolation_begin (SourceText src) _)     : xs -> replaceStringSegment loc src (("(" <>) . beginInterpolation) : go xs

      _ : xs -> go xs

    beginInterpolation src = init src <> "\" <> toString ("
    endInterpolation src = ") <> \"" <> tail src
    replaceStringSegment loc src f = Replace (Just loc.startColumn) loc.start loc.length (pack . unescapeString $ f src)
