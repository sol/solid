{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}

module Solid.PP.NewLexer where

import Prelude hiding (span, mod, takeWhile)

import Data.Char
import Data.Functor
import Data.Text (Text)
import Data.Text.Internal (Text(..))
import Data.Text.Unsafe (Iter(..))
import Data.Text.Internal.Encoding.Utf8
import qualified Data.Text.Unsafe as Unsafe
import qualified Data.Text as T

data Location = Location {
  offset :: !Int
, charOffset :: !Int
, line :: !Int
, column :: !Int
} deriving (Show, Eq) -- FIXME: Eq should not be used in production code; only compare offset instead

data SrcSpan = SrcSpan {
  start :: Location
, end   :: Location
} deriving (Show, Eq) -- FIXME: Eq should not be used in production code; only compare offset instead

data Tok = Tok {
  tokenType :: TokenType
, span :: SrcSpan
} deriving (Show, Eq)

textSpan :: Text -> Tok -> Text
textSpan input token = textSpan__ input token.span

textSpan__ :: Text -> SrcSpan -> Text
textSpan__ input span = textSpan_ input start end
  where
    start = span.start.offset
    end = span.end.offset

textSpan_ :: Text -> Int -> Int -> Text
textSpan_ (Text arr _ _) start end = Text arr start (end - start)

data TokenType =
    Keyword
  | Identifier
  | QualifiedIdentifier Text Text
  | Constructor
  | QualifiedConstructor Text Text
  | IncompleteQualifiedName Text
  | Operator Text
  | Integer
  | String
  | Symbol Char
  | Comment
  | EndOfFile
  deriving (Show, Eq)

data Lexer = Lexer {
  current :: Location
, input   :: Text
} deriving (Show, Eq)

data WithSrcSpan a = WithSrcSpan {
  span  :: SrcSpan
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
  pure $ WithSrcSpan (SrcSpan lexer.current end) match

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
      pure $ WithSrcSpan (SrcSpan start end) c

peekChar :: LexerM Char
peekChar = do
  lexer <- get
  return $ if T.null lexer.input then '\0' else T.head lexer.input

-- Lexer driver
tokenize :: Text -> [Tok]
tokenize input@(Text _ off _) = loop (Lexer (Location off 0 1 1) input)
  where
    loop :: Lexer -> [Tok]
    loop lexer = case lexOne.unLexerM lexer of
      (_, Tok EndOfFile _) -> []
      (new, token) -> token : loop new

lexOne :: LexerM Tok
lexOne = do
  lexer <- get
  mc <- peekChar
  case mc of
    c
      | c == '\0' -> return (Tok EndOfFile $ SrcSpan lexer.current lexer.current)
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
          pure $ Tok Identifier word.span

      | isUpper c -> do
          word <- takeWhile isIdChar
          pure $ Tok Constructor word.span
          -- LexerM qualifiedName

      | isDigit c -> do
          num <- takeWhile isDigit
          pure $ Tok Integer num.span

      | c == '"' -> do
          string
          new <- get
          pure $ Tok String (SrcSpan lexer.current new.current)

      | c `elem` operators -> do
          op <- takeWhile (`elem` operators)
          pure $ Tok (Operator op.value) op.span
      | c `elem` symbols -> do
          sym <- consumeChar
          pure $ Tok (Symbol sym.value) sym.span
      | otherwise -> do
          ch <- consumeChar
          pure $ Tok Comment ch.span -- FIXME

string :: LexerM (WithSrcSpan Char)
string = loop
  where
    loop = do
      _ <- consumeChar
      _ <- takeUntil (\ c -> c == '"' || c == '\\')
      c <- peekChar
      if
        | c == '"' -> consumeChar
        | c == '\\' -> consumeChar >> loop
        | otherwise -> undefined -- partial string - eof

qualifiedName :: Lexer -> (Lexer, Tok)
qualifiedName Lexer{..} = scanConstructor -1 0
  where
    scanConstructor :: Int -> Int -> (Lexer, Tok)
    scanConstructor lastDot !i
      | c == '.' = scanIdentifier (i + d)
      | isIdChar c = scanConstructor lastDot (i + d)
      | otherwise = done
      where
        done :: (Lexer, Tok)
        done = accept i \ match ->
          if lastDot < 0 then
            Constructor
          else
            QualifiedConstructor (Unsafe.takeWord8 (lastDot - 1) match) (Unsafe.dropWord8 lastDot match)

        Iter c d = safeIter input i

    scanIdentifier :: Int -> (Lexer, Tok)
    scanIdentifier !i
      | isLower c = accept (findEndOfId i) \ match ->
          let
            mod = Unsafe.takeWord8 (i - 1) match
            name = Unsafe.dropWord8 i match
            tok = QualifiedIdentifier mod name
          in tok
      | isIdChar c = scanConstructor i (i + d)
      | otherwise = accept i IncompleteQualifiedName
      where
        Iter c d = safeIter input i

    findEndOfId :: Int -> Int
    findEndOfId !i
      | isIdChar c = findEndOfId (i + d)
      | otherwise = i
      where
        Iter c d = safeIter input i

    accept :: Int -> (Text -> TokenType) -> (Lexer, Tok)
    accept n f =
      let
        match = Unsafe.takeWord8 n input
        rest = Unsafe.dropWord8 n input
        new = Lexer {
          current = advanceText current match
        , input = rest
        }
      in (new, Tok (f match) (SrcSpan current new.current))


isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_'

-- helpers

keywords :: [Text]
keywords =
  [ "case", "class", "data", "default", "deriving", "do", "else", "if"
  , "import", "in", "infix", "infixl", "infixr", "instance", "let"
  , "module", "newtype", "of", "then", "type", "where", "forall"
  ]

operators :: [Char]
operators = ":!#$%&*+./<=>?@\\^|-~"

symbols :: [Char]
symbols = "(),;[]{}"

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
