{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Solid.PP (
  Result(..)
, run
, extensions
) where

import           Prelude ()
import           Solid.PP.IO hiding (concatMap)

import           Data.Char
import           Data.Word
import qualified Data.Text as T
import qualified Data.ByteString.Short as SB

import           Solid.PP.DList
import           Solid.PP.Edit
import           Solid.PP.Lexer
import           Solid.PP.Parser

extensions :: [Extension]
extensions = [
    DataKinds
  , DeriveAnyClass
  , OverloadedRecordDot
  , OverloadedStrings
  ]

data Result = Failure String | Success
  deriving (Eq, Show)

run :: FilePath -> FilePath -> FilePath -> IO Result
run src cur dst = do
  input <- readFile cur
  preProcesses src dst $ addLinePragma input
  where
    addLinePragma = (linePragma <>)
    linePragma = "{-# LINE 1 " <> pack (show src) <> " #-}\n"

preProcesses :: FilePath -> FilePath -> Text -> IO Result
preProcesses src dst input = case parse extensions src input of
  Left err -> return (Failure err)
  Right nodes -> withFile dst WriteMode $ \ h -> do
    edit h input (pp nodes)
    return Success

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
  | otherwise = (Replace (Just loc.startColumn) loc.start loc.length (pack new) :)
  where
    new = unescapeString old

pp :: [Node] -> [Edit]
pp = ($ []) . onNodes
  where
    onNodes :: [Node] -> [Edit] -> [Edit]
    onNodes = concatMap onNode

    onNode :: Node -> [Edit] -> [Edit]
    onNode = \ case
      Token loc token -> onToken loc token
      LiteralString string -> onLiteralString string

    onToken :: BufferSpan -> Token -> [Edit] -> [Edit]
    onToken loc = \ case
      ITvarid identifier -> desugarIdentifier loc.start loc.end identifier
      ITqvarid (succ . lengthFS -> offset, identifier) -> desugarIdentifier (loc.start + offset) loc.end identifier
      _ -> id

    onLiteralString :: LiteralString BufferSpan -> [Edit] -> [Edit]
    onLiteralString = \ case
      Literal loc src -> unescapeStringLiteral loc src
      Begin loc src expression -> replaceStringSegment loc ("(\"" <> unescape src <> beginInterpolation) . onExpression expression

    onExpression :: Expression -> [Edit] -> [Edit]
    onExpression = \ case
      Expression nodes end -> onNodes nodes . onEnd end

    onEnd :: End BufferSpan -> [Edit] -> [Edit]
    onEnd = \ case
      End loc src -> replaceStringSegment loc (endInterpolation <> unescape src <> "\")")
      EndBegin loc src expression -> replaceStringSegment loc (endInterpolation <> unescape src <> beginInterpolation) . onExpression expression

    unescape :: String -> DString
    unescape = fromString . unescapeString . init . tail

    beginInterpolation :: DString
    beginInterpolation = "\" <> toString ("

    endInterpolation :: DString
    endInterpolation = ") <> \""

    replaceStringSegment :: BufferSpan -> DString -> [Edit] -> [Edit]
    replaceStringSegment loc src = (Replace (Just loc.startColumn) loc.start loc.length (pack src.build) :)

concatMap :: Foldable t => (a -> [b] -> [b]) -> t a -> [b] -> [b]
concatMap f = foldr ((.) . f) id
