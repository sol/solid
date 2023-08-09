{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.PP.Parser (
  parse

, Node
, Expression

, NodeWith(..)
, LiteralString(..)
, ExpressionWith(..)
, End(..)
) where

import           Prelude ()
import           Solid.PP.IO hiding (error)

import           Solid.PP.Lexer hiding (Token)
import qualified Solid.PP.Lexer as Lexer

deriving instance Eq Lexer.Token

type Token = WithBufferSpan Lexer.Token

type Result = Either Error

data Error = UnterminatedStringInterpolation

error :: Error -> Result a
error = Left

renderError :: SrcLoc -> Error -> String
renderError loc = (renderLoc loc <>) . \ case
  UnterminatedStringInterpolation -> "unterminated string interpolation"

renderLoc :: SrcLoc -> String
renderLoc loc = loc.file <> ":" <> show loc.line <> ":" <> show loc.column <> ": "

parse :: [LanguageFlag] -> FilePath -> Text -> Either String [Node]
parse extensions src input = do
  result <- tokenize extensions src input
  case parseNodes result.tokens of
    Left err -> Left $ renderError result.end err
    Right r -> Right r

parseNodes :: [Token] -> Result [Node]
parseNodes = parseNodeThen onNode onEndOfInput
  where
    onNode :: Node -> [Token] -> Result [Node]
    onNode node tokens = (node :) <$> parseNodes tokens

    onEndOfInput :: Result [Node]
    onEndOfInput = return []

parseNodeThen :: (Node -> [Token] -> Result r) -> Result r -> [Token] -> Result r
parseNodeThen onNode onEndOfInput = \ case
  TokenBegin loc src : tokens -> interpolatedStringBegin loc src onNode tokens
  L loc (ITstring (SourceText src) _) : tokens -> onNode (LiteralString $ Literal loc src) tokens
  L loc token : tokens -> onNode (Token loc token) tokens
  [] -> onEndOfInput

interpolatedStringBegin :: BufferSpan -> String -> (Node -> [Token] -> Result r) -> [Token] -> Result r
interpolatedStringBegin loc src = beginInterpolation (LiteralString . Begin loc src)

beginInterpolation :: forall inter r. (Expression -> inter) -> (inter -> [Token] -> Result r) -> [Token] -> Result r
beginInterpolation cont = parseExpression $ \ nodes end -> cont (Expression nodes end)

parseExpression :: forall inter r. ([Node] -> End BufferSpan -> inter) -> (inter -> [Token] -> Result r) -> [Token] -> Result r
parseExpression onExpressionDone onStringDone = \ case
  TokenEnd loc src : tokens -> onStringDone (onExpressionDone [] (End loc src)) tokens
  TokenEndBegin loc src : tokens -> beginInterpolation (onExpressionDone [] . EndBegin loc src) onStringDone tokens
  tokens -> parseNodeThen moreExpressionsNodes onEndOfInput tokens
    where
      moreExpressionsNodes :: Node -> [Token] -> Result r
      moreExpressionsNodes node = parseExpression (onExpressionDone . (node :)) onStringDone

      onEndOfInput :: Result r
      onEndOfInput = error UnterminatedStringInterpolation

pattern TokenBegin :: BufferSpan -> String -> Token
pattern TokenBegin loc src <- L loc (ITstring_interpolation_begin (SourceText src) _)

pattern TokenEnd :: BufferSpan -> String -> Token
pattern TokenEnd loc src <- L loc (ITstring_interpolation_end (SourceText src) _)

pattern TokenEndBegin :: BufferSpan -> String -> Token
pattern TokenEndBegin loc src <- L loc (ITstring_interpolation_end_begin (SourceText src) _)

type Node = NodeWith BufferSpan
type Expression = ExpressionWith BufferSpan

data NodeWith loc = Token loc Lexer.Token | LiteralString (LiteralString loc)
  deriving (Eq, Show, Functor)

data LiteralString loc = Literal loc String | Begin loc String (ExpressionWith loc)
  deriving (Eq, Show, Functor)

data ExpressionWith loc = Expression [NodeWith loc] (End loc)
  deriving (Eq, Show, Functor)

data End loc = End loc String | EndBegin loc String (ExpressionWith loc)
  deriving (Eq, Show, Functor)

instance HasField "loc" (End loc) loc where
  getField = \ case
    End loc _ -> loc
    EndBegin loc _ _ -> loc
