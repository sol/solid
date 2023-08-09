{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
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
import           Solid.PP.IO hiding (try, error, some, many)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Text.Megaparsec as P (parse, token)
import           Text.Megaparsec hiding (Token, token, parse, some)

import           Solid.PP.Lexer hiding (Token)
import qualified Solid.PP.Lexer as Lexer

import qualified GHC.Types.Basic as GHC
import qualified GHC.Hs.DocString as GHC
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Data.FastString as GHC

deriving instance Eq Lexer.Token

instance Ord FastString where
  compare = GHC.lexicalCompareFS

instance Eq FastZString where
  a == b = GHC.fastZStringToByteString a == GHC.fastZStringToByteString b

instance Ord FastZString where
  compare a b = compare (GHC.fastZStringToByteString a) (GHC.fastZStringToByteString b)

deriving instance Ord Lexer.Token
deriving instance Ord GHC.InlineSpec
deriving instance Ord SourceText
deriving instance Ord GHC.RuleMatchInfo
deriving instance Ord GHC.HsDocString
deriving instance Ord GHC.SrcSpan
deriving instance Ord GHC.UnhelpfulSpanReason

type Parser = Parsec Error [Token]

type Token = WithBufferSpan Lexer.Token

data Error = UnterminatedStringInterpolation
  deriving (Eq, Show, Ord)

error :: Error -> Parser a
error = fancyFailure . Set.singleton . ErrorCustom

renderError :: SrcLoc -> Error -> String
renderError loc = (renderLoc loc <>) . \ case
  UnterminatedStringInterpolation -> "unterminated string interpolation"

renderLoc :: SrcLoc -> String
renderLoc loc = loc.file <> ":" <> show loc.line <> ":" <> show loc.column <> ": "

parse :: [LanguageFlag] -> FilePath -> Text -> Either String [Node]
parse extensions src input = do
  result <- tokenize extensions src input
  case P.parse (many pNode) src result.tokens of
    Left err -> Left $ case NonEmpty.head err.bundleErrors of
      FancyError _ (Set.toList -> ErrorCustom e : _) -> renderError result.end e
      _ -> show err
    Right r -> Right r

token :: (Token -> Maybe a) -> Parser a
token = (`P.token` mempty)

pNode :: Parser Node
pNode = pLiteralString <|> pInterpolatedString <|> pAnyToken

pLiteralString :: Parser Node
pLiteralString = token \ case
  L loc (ITstring (SourceText src) _) -> Just $ LiteralString (Literal loc src)
  _ -> Nothing

pInterpolatedString :: Parser Node
pInterpolatedString = pTokenBegin <*> pExpression
  where
    pExpression :: Parser Expression
    pExpression = Expression <$> many pNode <*> (pEnd <|> error UnterminatedStringInterpolation)

    pEnd :: Parser (End BufferSpan)
    pEnd = pTokenEnd <|> pTokenEndBegin <*> pExpression

pAnyToken :: Parser Node
pAnyToken = token \ case
  TokenEndBegin {} -> Nothing
  TokenEnd {} -> Nothing
  L loc t -> Just $ Token loc t

pTokenBegin :: Parser (Expression -> Node)
pTokenBegin = token \ case
  TokenBegin loc src -> Just $ LiteralString . Begin loc src
  _ -> Nothing

pTokenEnd :: Parser (End BufferSpan)
pTokenEnd = token \ case
  TokenEnd loc src -> Just (End loc src)
  _ -> Nothing

pTokenEndBegin :: Parser (Expression -> End BufferSpan)
pTokenEndBegin = token \ case
  TokenEndBegin loc src -> Just (EndBegin loc src)
  _ -> Nothing

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
