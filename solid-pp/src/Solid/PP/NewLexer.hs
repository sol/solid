{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}

module Solid.PP.NewLexer (
  Span(..)
, Token(..)
, TokenType(..)
, synthesize
, tokenize
, tokenText
, textSpan
) where

import Control.Monad (unless)
import Solid.PP.IO (pass)
import Prelude hiding (span, mod, takeWhile)

import Data.Char (chr, isDigit, isAsciiLower, isAsciiUpper)
import Data.Text (Text)
import Data.Text.Internal (Text(..))
import qualified Data.Text as T
import Control.Monad.Trans.State

import Solid.PP.NewLexer.Type (Lexer)
import qualified Solid.PP.NewLexer.Type as Lexer

data Span = Span {
  start :: Int
, end :: Int
} deriving (Show, Eq)

data Token = Token {
  tokenType :: TokenType
, span :: Span
} deriving (Show, Eq)

tokenText :: Text -> Token -> Text
tokenText input token = textSpan input token.span

textSpan :: Text -> Span -> Text
textSpan input span = textRange input span.start span.end

textRange :: Text -> Int -> Int -> Text
textRange (Text arr _ _) start end = Text arr start (end - start)

unsafeTakeBytes :: Int -> Text -> Text
unsafeTakeBytes n (Text arr off _) = Text arr off n

data TokenType =
    -- Keyword
    Constructor
  | Identifier
  | Symbol
  | Integer
  | Char
  | UnterminatedChar
  | String
  | UnterminatedString
  | Comment
  | Pragma
  | UnterminatedPragma
  | Backtick

  | Comma
  | Semicolon
  | ParenOpen
  | ParenClose
  | BracketOpen
  | BracketClose
  | BraceOpen
  | BraceClose

  | EndOfFile

  -- synthetic tokens
  | QualifiedIdentifier
  | QualifiedConstructor
  | IncompleteQualifiedName
  | Projection
  deriving (Show, Eq)

type LexerM = State Lexer

tokenize :: Text -> [Token]
tokenize input = loop (Lexer.new input)
  where
    loop :: Lexer -> [Token]
    loop lexer = case runState nextToken lexer of
      (Token EndOfFile _, _) -> []
      (token, new) -> token : loop new

unsafeAdvance :: LexerM ()
unsafeAdvance = unsafeAdvanceBy 1

unsafeAdvanceBy :: Int -> LexerM ()
unsafeAdvanceBy = modify . Lexer.unsafeAdvance

advanceBy :: Int -> LexerM ()
advanceBy = modify . Lexer.advance

peek :: LexerM Char
peek = gets $ Lexer.index 0

peekInput :: LexerM Text
peekInput = gets (.input)

advanceWhile :: (Char -> Bool) -> LexerM ()
advanceWhile = modify . Lexer.advanceWhile

advanceUntil :: (Char -> Bool) -> LexerM ()
advanceUntil p = advanceWhile (not . p)

next :: LexerM Char
next = state Lexer.next

nextToken :: LexerM Token
nextToken = do
  lexer <- get

  let
    start :: Int
    start = lexer.offset

    accept :: TokenType -> LexerM Token
    accept t = do
      end <- (.offset) <$> get
      return $ Token t (Span start end)

  next >>= \ c -> if
      | c == '\0' -> do
          return (Token EndOfFile $ Span start start)

      | isSpace c -> do
          nextToken

      | c == '`' -> do
          accept Backtick

      | isAsciiLower c || c == '_' -> do
          advanceWhile isIdChar
          accept Identifier

      | isAsciiUpper c -> do
          advanceWhile isIdChar
          accept Constructor

      | isDigit c -> do
          advanceWhile isDigit
          accept Integer

      | c == '\'' -> do
          char >>= accept

      | c == '"' -> do
          string >>= accept

      | isSymbol c -> do
          advanceWhile isSymbol
          new <- get
          let n = new.offset - start
          case unsafeTakeBytes n lexer.input of
            "--" -> do
              advanceWhile (/= '\n')
              nextToken
            _ -> accept Symbol

      | c == '{' && Lexer.index 1 lexer == '-' -> do
          unsafeAdvance

          peek >>= \ case
            '#' -> do
              unsafeAdvance
              let
                findEnd = peekInput >>= \ case
                  "" -> accept UnterminatedPragma
                  input | "#-}" `T.isPrefixOf` input -> unsafeAdvanceBy 3 >> accept Pragma
                  _ -> unsafeAdvance >> findEnd
              findEnd

            _ -> do
              let
                go :: Int -> LexerM ()
                go n = do
                  advanceUntil (\ ch -> ch == '-' || ch == '{')
                  peek >>= \ case
                    '\0' -> pass
                    '{' -> do
                      unsafeAdvance
                      peek >>= \ case
                        -- '\0' -> pass
                        '-' -> go (n + 1)
                        _ -> go n
                    _ -> do
                      unsafeAdvance
                      peek >>= \ case
                        '}' -> do
                          unsafeAdvance
                          unless (n == 0) do
                            go (n - 1)
                        _ -> go n
              go 0
              nextToken

      | c == ',' -> accept Comma
      | c == ';' -> accept Semicolon
      | c == '(' -> accept ParenOpen
      | c == ')' -> accept ParenClose
      | c == '[' -> accept BracketOpen
      | c == ']' -> accept BracketClose
      | c == '{' -> accept BraceOpen
      | c == '}' -> accept BraceClose

      | otherwise -> accept Comment

char :: LexerM TokenType
char = loop
  where
    loop :: LexerM TokenType
    loop = do
      advanceUntil \ c -> c == '\'' || c == '\\' || c == '\n'
      peek >>= \ case
        '\''  -> unsafeAdvance >> pure Char
        '\\' -> advanceBy 2 >> loop
        _ -> pure UnterminatedChar

string :: LexerM TokenType
string = loop
  where
    loop :: LexerM TokenType
    loop = do
      advanceUntil \ c -> c == '"' || c == '\\' || c == '\n'
      peek >>= \ case
        '"'  -> unsafeAdvance >> pure String
        '\\' -> advanceBy 2 >> loop
        _ -> pure UnterminatedString

isIdChar :: Char -> Bool
isIdChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '\'' || c > chr 127

isSymbol :: Char -> Bool
isSymbol = (`elem` symbols)

symbols :: [Char]
symbols = ":!#$%&*+./<=>?@\\^|-~"

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\n'

synthesize :: Text -> [Token] -> [Token]
synthesize input = loop
  where
    text :: Span -> Text
    text = textSpan input

    loop :: [Token] -> [Token]
    loop = \ case
      [] -> []
      Token Symbol start@(text -> ".") : Token Identifier end : rest | start.end == end.start -> Token Projection (Span start.start end.end) : loop rest

      Token Constructor start : Token Symbol end@(text -> ".") : rest | start.end == end.start -> qualifiedName (union start end) rest
      token : rest -> token : loop rest

    qualifiedName :: Span -> [Token] -> [Token]
    qualifiedName start = \ case
      Token Constructor name : Token Symbol end@(text -> ".") : rest | name.end == end.start -> qualifiedName span rest
        where
          span = union start end

      Token t end : rest | start.end == end.start -> case t of
        Constructor -> accept QualifiedConstructor
        Identifier -> accept QualifiedIdentifier
        _ -> undefined
        where
          accept tt = Token tt (Span start.start end.end) : loop rest
      tokens@(_ : _) -> Token IncompleteQualifiedName start : loop tokens
      [] -> [Token IncompleteQualifiedName start]

union :: Span -> Span -> Span
union start end = Span start.start end.end
