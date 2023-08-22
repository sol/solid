{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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

import           Data.List hiding (lines)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Text.Megaparsec as P
import           Text.Megaparsec hiding (Token, token, tokens, parse, parseTest, some)

import           Solid.PP.Lexer hiding (Token, tokens)
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

type Parser = Parsec Error TokenStream

type Token = WithBufferSpan Lexer.Token

data TokenStream = TokenStream {
  original :: Text
, tokens :: [Token]
}

instance Stream TokenStream where
  type Token TokenStream = Token
  type Tokens TokenStream = [Token]

  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null

  take1_ (TokenStream original tokens) = case tokens of
    [] -> Nothing
    t : ts -> Just (t, TokenStream original ts)

  takeN_ n stream@(TokenStream original tokens)
    | n <= 0 = Just ([], stream)
    | null tokens = Nothing
    | otherwise = Just $ TokenStream original <$> splitAt n tokens

  takeWhile_ p stream = TokenStream stream.original <$> span p stream.tokens

instance VisualStream TokenStream where
  showTokens :: Proxy TokenStream -> NonEmpty Token -> String
  showTokens Proxy = intercalate " " . map (showToken . unLoc) . NonEmpty.toList

instance TraversableStream TokenStream where
  reachOffset offset (reachOffsetNoLine offset -> state) = (sourceLine, state)
    where
      sourceLine :: Maybe String
      sourceLine = getSourceLine state.pstateSourcePos.sourceLine state.pstateInput.original

      getSourceLine :: Pos -> Text -> Maybe String
      getSourceLine (pred . unPos -> n) = fmap unpack . listToMaybe . drop n . lines

  reachOffsetNoLine n PosState{..} = PosState {
      pstateInput = pstateInput { tokens }
    , pstateOffset = offset
    , pstateSourcePos = sourcePos
    , pstateTabWidth = pstateTabWidth
    , pstateLinePrefix = pstateLinePrefix
    }
    where
      offset :: Int
      offset = max pstateOffset n

      tokens :: [Token]
      tokens = drop (offset - pstateOffset) pstateInput.tokens

      sourcePos :: SourcePos
      sourcePos = case tokens of
        [] -> case reverse pstateInput.tokens of
          L loc _ : _ -> loc.endLoc.toSourcePos
          [] -> pstateSourcePos
        (L loc _ : _) -> loc.startLoc.toSourcePos

instance HasField "toSourcePos" SrcLoc SourcePos where
  getField loc = SourcePos loc.file (mkPos loc.line) (mkPos loc.column)

showToken :: Lexer.Token -> String
showToken = \ case
  ITcbrack -> "]"
  t -> show t

instance ShowErrorComponent Error where
  showErrorComponent = \ case
    UnterminatedStringInterpolation -> "unterminated string interpolation"

data Error = UnterminatedStringInterpolation
  deriving (Eq, Show, Ord)

error :: Error -> Parser a
error = fancyFailure . Set.singleton . ErrorCustom

parse :: [LanguageFlag] -> FilePath -> Text -> Either String [Node]
parse extensions src input = do
  result <- tokenize extensions src input
  let
    stream :: TokenStream
    stream = TokenStream input result.tokens
  case P.parse (many pNode <* eof) src stream of
    Left err -> Left $ errorBundlePretty err
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

data NodeWith loc =
    Token loc Lexer.Token
  | LiteralString (LiteralString loc)
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
