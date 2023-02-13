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

desugarIdentifier :: Int -> Int -> FastString -> DList Edit
desugarIdentifier start end identifier
  | lastChar == qmark || lastChar == bang = replace replacement
  | otherwise = mempty
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

    replace :: Text -> DList Edit
    replace = singleton . Replace Nothing start (end - start)

replaceBufferSpan :: BufferSpan -> String -> Edit
replaceBufferSpan loc = Replace (Just loc.startColumn) loc.start loc.length . pack

unescapeString :: String -> String
unescapeString = go
  where
    go = \ case
      [] -> []
      '\\' : '{' : xs -> '{' : go xs
      x : xs -> x : go xs

unescapeStringLiteral :: BufferSpan -> String -> DList Edit
unescapeStringLiteral loc old
  | new == old = mempty
  | otherwise = singleton $ replaceBufferSpan loc new
  where
    new = unescapeString old

pp :: [Node] -> [Edit]
pp = (.build) . onNodes
  where
    onNodes :: [Node] -> DList Edit
    onNodes = concatMap onNode

    onNode :: Node -> DList Edit
    onNode = \ case
      Token loc token -> onToken loc token
      LiteralString string -> onLiteralString string

    onToken :: BufferSpan -> Token -> DList Edit
    onToken loc = \ case
      ITvarid identifier -> desugarIdentifier loc.start loc.end identifier
      ITqvarid (succ . lengthFS -> offset, identifier) -> desugarIdentifier (loc.start + offset) loc.end identifier
      _ -> mempty

    onLiteralString :: LiteralString BufferSpan -> DList Edit
    onLiteralString = \ case
      Literal loc src -> unescapeStringLiteral loc src
      Begin loc src expression -> replace loc (lambdaAbstract expression <> unescape src <> beginInterpolation) <> onExpression 1 expression

    onExpression :: Int -> Expression -> DList Edit
    onExpression n = \ case
      Expression [] end -> insert end.loc (abstractionParam n "") <> onEnd (succ n) end
      Expression nodes end -> onNodes nodes <> onEnd n end

    onEnd :: Int -> End BufferSpan -> DList Edit
    onEnd n = \ case
      End loc src -> replace loc (endInterpolation <> unescape src <> "\")")
      EndBegin loc src expression -> replace loc (endInterpolation <> unescape src <> beginInterpolation) <> onExpression n expression

    unescape :: String -> DString
    unescape = fromString . unescapeString . init . tail

    beginInterpolation :: DString
    beginInterpolation = "\" <> toString ("

    endInterpolation :: DString
    endInterpolation = ") <> \""

    replace :: BufferSpan -> DString -> DList Edit
    replace loc = singleton . replaceBufferSpan loc . (.build)

    insert :: BufferSpan -> String -> DList Edit
    insert loc = singleton . Replace (Just loc.startColumn) loc.start 0 . pack

lambdaAbstract :: Expression -> DString
lambdaAbstract = lambda . countAbstractions
  where
    lambda :: Int -> DString
    lambda n
      | n == 0 = "(\""
      | otherwise = "(\\" <> params <> " -> \""
      where
        params :: DString
        params = concatMap formatParam [1..n]

    formatParam :: Int -> DString
    formatParam n = DList $ (' ' :) . abstractionParam n

    countAbstractions :: Expression -> Int
    countAbstractions = onExpression
      where
        onExpression :: Expression -> Int
        onExpression = \ case
          Expression nodes end -> onEnd end + case nodes of
            [] -> 1
            _ -> 0

        onEnd :: End BufferSpan -> Int
        onEnd = \ case
          End _ _ -> 0
          EndBegin _ _ expression -> onExpression expression

abstractionParam :: Int -> ShowS
abstractionParam n = ('_' :) . shows n
