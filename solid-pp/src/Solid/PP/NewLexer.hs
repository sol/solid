{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}

module Solid.PP.NewLexer where

import Data.ByteString.Internal (w2c)
import Prelude hiding (span, mod, takeWhile)

import Data.Char hiding (isSymbol)
import Data.Functor
import Data.Text (Text)
import Data.Text.Internal (Text(..))
import Data.Text.Unsafe (Iter(..))
import Data.Text.Internal.Encoding.Utf8
import qualified Data.Text.Unsafe as Unsafe
import qualified Data.Text as T
import qualified Data.Text.Array as Array

data Location = Location {
  offset :: !Int
, charOffset :: !Int
, line :: !Int
, column :: !Int
} deriving (Show, Eq) -- FIXME: Eq should not be used in production code; only compare offset instead

adjustOffset :: Int -> Location -> Location
adjustOffset n Location{..} = Location {
  offset = offset + n
, charOffset = charOffset + n
, line
, column = column + n
}
{-# INLINE adjustOffset #-}

data Span = Span {
  start :: Location
, end   :: Location
} deriving (Show, Eq) -- FIXME: Eq should not be used in production code; only compare offset instead

data Token = Token {
  tokenType :: TokenType
, span :: Span
} deriving (Show, Eq)

textSpan :: Text -> Token -> Text
textSpan input token = textSpan__ input token.span

textSpan__ :: Text -> Span -> Text
textSpan__ input span = textSpan_ input start end
  where
    start = span.start.offset
    end = span.end.offset

textSpan_ :: Text -> Int -> Int -> Text
textSpan_ (Text arr _ _) start end = Text arr start (end - start)

data TokenType =
    -- Keyword
    Constructor
  | Identifier
  | Symbol Text
  | Integer
  | String
  | UnterminatedString
  | Special Char
  | Comment
  | EndOfFile

  -- synthetic tokens
  | QualifiedIdentifier
  | QualifiedConstructor
  | IncompleteQualifiedName
  | Projection
  deriving (Show, Eq)

union :: Span -> Span -> Span
union start end = Span start.start end.end

synthesize :: [Token] -> [Token]
synthesize = loop
  where
    loop :: [Token] -> [Token]
    loop = \ case
      [] -> []
      Token (Symbol ".") start : Token Identifier end : rest | start.end.offset == end.start.offset -> Token Projection (Span start.start end.end) : loop rest
      Token Constructor start : Token (Symbol ".") end : rest | start.end.offset == end.start.offset -> qualifiedName (union start end) rest
      token : rest -> token : loop rest

    qualifiedName :: Span -> [Token] -> [Token]
    qualifiedName start = \ case
      Token Constructor name : Token (Symbol ".") end : rest | name.end.offset == end.start.offset -> qualifiedName span rest
        where
          span = union start end

      Token t end : rest | start.end.offset == end.start.offset -> case t of
        Constructor -> accept QualifiedConstructor
        Identifier -> accept QualifiedIdentifier
        _ -> undefined
        where
          accept tt = Token tt (Span start.start end.end) : loop rest
      tokens@(_ : _) -> Token IncompleteQualifiedName start : loop tokens
      [] -> [Token IncompleteQualifiedName start]

data Lexer = Lexer {
  current :: Location
, input   :: Text
} deriving (Show, Eq)

data WithSrcSpan a = WithSrcSpan {
  span  :: Span
, value :: a
} deriving (Show, Eq)

newtype LexerM a = LexerM { unLexerM :: Lexer -> (Lexer, a) }

instance Functor LexerM where
  fmap f (LexerM m) = LexerM \s -> let (s', a) = m s in (s', f a)

instance Applicative LexerM where
  pure a = LexerM \s -> (s, a)
  (LexerM mf) <*> (LexerM ma) = LexerM \s ->
    let (s', f) = mf s
        (s'', a) = ma s'
    in (s'', f a)

instance Monad LexerM where
  (LexerM ma) >>= f = LexerM \s ->
    let (s', a) = ma s
        LexerM mb = f a
    in mb s'

-- primitives
get :: LexerM Lexer
get = LexerM \s -> (s, s)

put :: Lexer -> LexerM ()
put s = LexerM \_ -> (s, ())

modify :: (Lexer -> Lexer) -> LexerM ()
modify f = LexerM \s -> (f s, ())

-- Lexer combinators

takeWhile :: (Char -> Bool) -> LexerM (WithSrcSpan Text)
takeWhile p = do
  lexer <- get
  let (match, rest) = T.span p lexer.input
      end = advanceText lexer.current match
  put $ Lexer end rest
  pure $ WithSrcSpan (Span lexer.current end) match

takeUntil :: (Char -> Bool) -> LexerM (WithSrcSpan Text)
takeUntil p = takeWhile (not . p)

consumeChar :: LexerM (WithSrcSpan Char)
consumeChar = do
  lexer <- get
  case T.uncons lexer.input of
    Nothing -> error "Unexpected EOF in consumeChar"
    Just (c, rest) -> do
      let start = lexer.current
          end = advanceChar start c
      put $ Lexer end rest
      pure $ WithSrcSpan (Span start end) c

consumeChar_ :: LexerM ()
consumeChar_ = void consumeChar

index :: Text -> Int -> Char
index (Text arr off len) n
  | n < len = w2c $ Array.unsafeIndex arr (off + n)
  | otherwise = '\0'
{-# INLINE index #-}

peekAt :: Int -> LexerM Char
peekAt n = do
  lexer <- get
  return $ index lexer.input n

peek :: LexerM Char
peek = do
  lexer <- get
  return $ if T.null lexer.input then '\0' else T.head lexer.input

-- Lexer driver
tokenize :: Text -> [Token]
tokenize input@(Text _ off _) = loop (Lexer (Location off 0 1 1) input)
  where
    loop :: Lexer -> [Token]
    loop lexer = case lexOne.unLexerM lexer of
      (_, Token EndOfFile _) -> []
      (new, token) -> token : loop new

lexOne :: LexerM Token
lexOne = do
  lexer <- get
  mc <- peek
  case mc of
    c
      | c == '\0' -> return (Token EndOfFile $ Span lexer.current lexer.current)
    {-
      | T.isPrefixOf "--" <$> (input <$> get) -> do
          comment <- takeUntil (== '\n')
          pure $ Token (TokComment comment.value) comment.span
          -}
      | isSpace c -> do
          spaces <- takeWhile isSpace
          lexOne

      | isLower c || c == '_' -> do
          word <- takeWhile isIdChar
          {-
          let typ = if word.value `elem` keywords
                      then TokKeyword word.value
                      else Identifier word.value
          pure $ Token typ word.span
          -}
          pure $ Token Identifier word.span

      | isUpper c -> do
          word <- takeWhile isIdChar
          pure $ Token Constructor word.span

      | isDigit c -> do
          num <- takeWhile isDigit
          pure $ Token Integer num.span

      | c == '"' -> do
          t <- string
          new <- get
          pure $ Token t (Span lexer.current new.current)

      | isSymbol c -> do
          op <- takeWhile isSymbol
          case op.value of
            "--" -> do

              -- FIXME: improve performance
              --
              -- 1. don't need to update column
              -- 2. skip \n and increase line
              ignore <- takeWhile (/= '\n')

              lexOne
            _ -> pure $ Token (Symbol op.value) op.span

      | c `elem` special -> do
          sym <- consumeChar
          pure $ Token (Special sym.value) sym.span
      | otherwise -> do
          ch <- consumeChar
          pure $ Token Comment ch.span -- FIXME

string :: LexerM TokenType
string = loop
  where
    loop :: LexerM TokenType
    loop = do
      consumeChar_
      _ <- takeUntil (\ c -> c == '"' || c == '\\' || c == '\n')
      c <- peek
      if
        | c == '"'  -> consumeChar_ >> pure String
        | c == '\\' -> consumeChar_ >> loop
        | c == '\n' -> pure UnterminatedString
        | otherwise -> pure UnterminatedString

isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_'

-- helpers

keywords :: [Text]
keywords =
  [ "case", "class", "data", "default", "deriving", "do", "else", "if"
  , "import", "in", "infix", "infixl", "infixr", "instance", "let"
  , "module", "newtype", "of", "then", "type", "where", "forall"
  ]

symbols :: [Char]
symbols = ":!#$%&*+./<=>?@\\^|-~"


-- .. | : | :: | = | \ | | | <- | -> | @ | ~ | =>
reservedop =  ["..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

isSymbol :: Char -> Bool
isSymbol = (`elem` symbols)

special :: [Char]
special = "(),;[]{}"

advanceChar :: Location -> Char -> Location
advanceChar (Location offset o l c) ch
  | ch == '\n' = Location (offset + utf8Length ch) (o + 1) (l + 1) 1
  | otherwise  = Location (offset + utf8Length ch) (o + 1) l (c + 1)

advanceText :: Location -> Text -> Location
advanceText = T.foldl' advanceChar

safeIter :: Text -> Int -> Iter
safeIter input@(Text _ _ len) i
  | i >= len = Iter '\0' 0
  | otherwise = Unsafe.iter input i
{-# INLINE safeIter #-}
