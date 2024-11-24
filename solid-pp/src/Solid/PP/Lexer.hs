{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}
module Solid.PP.Lexer (
  LexerResult(..)
, Token(.., InfixProjection, PrefixProjection)
, SourceError
, tokenize
, tokenizeWithComments

, StringBuffer
, stringToStringBuffer

, module FastString
, module GHC.Types.SourceText

, module Solid.PP.SrcLoc
) where

import           Prelude ()
import           Solid.PP.IO

import           Data.Function

import           Solid.PP.SrcLoc
import           Solid.PP.Lexer.Extensions

import           Lexer hiding (lexTokenStream)
import           GHC.Data.FastString as FastString
import           GHC.Types.SourceText
import           GHC.Data.StringBuffer hiding (cur)
import           GHC.Types.SrcLoc hiding (SrcLoc)
import           GHC.Driver.Errors.Types
import           GHC.Types.SourceError

data LexerResult = LexerResult {
  tokens :: [WithBufferSpan Token]
, end :: SrcLoc
, errors :: String
} deriving Show

pattern InfixProjection :: Token
pattern InfixProjection = ITproj False

pattern PrefixProjection :: Token
pattern PrefixProjection = ITproj True

change_any_projections_after_a_trailing_bang_to_infix :: [WithBufferSpan Token] -> [WithBufferSpan Token]
change_any_projections_after_a_trailing_bang_to_infix = fix $ \ rec -> \ case
  identifier@(L identifier_loc ITvarid {}) : L loc PrefixProjection : tokens | identifier_loc.end == loc.start -> identifier : L loc InfixProjection : rec tokens
  token : tokens -> token : rec tokens
  [] -> []

tokenize :: Language -> [ExtensionFlag] -> FilePath -> Int -> Text -> Either String LexerResult
tokenize = tokenize_ False

tokenizeWithComments :: Language -> [ExtensionFlag] -> FilePath -> Int -> Text -> Either String LexerResult
tokenizeWithComments = tokenize_ True

tokenize_ :: Bool -> Language -> [ExtensionFlag] -> FilePath -> Int -> Text -> Either String LexerResult
tokenize_ keepComments language extensionFlags src line input = do
  case lexTokenStream opts buffer loc of
    POk state tokens -> return LexerResult {
      tokens = change_any_projections_after_a_trailing_bang_to_infix tokens
    , end = fromPsLoc state.loc
    , errors = errors state
    }

    PFailed state -> Left (errors state)
  where
    errors :: PState -> String
    errors = show . getErrors

    opts :: ParserOpts
    opts = makeOpts keepComments (not keepComments) extensions

    extensions :: EnumSet Extension
    extensions = extensionsFromModuleHeader (Just language) extensionFlags src buffer

    loc :: RealSrcLoc
    loc = mkRealSrcLoc (mkFastString src) line 1

    buffer :: StringBuffer
    buffer = stringBufferFromByteString $ encodeUtf8 input

    getErrors :: PState -> SourceError
    getErrors = mkSrcErr . fmap GhcPsMessage . getPsErrorMessages

lexTokenStream :: ParserOpts -> StringBuffer -> RealSrcLoc -> ParseResult [WithBufferSpan Token]
lexTokenStream opts buf loc = unP go (initParserState opts buf loc)
  where
    queueComments = False

    go :: P [WithBufferSpan Token]
    go = do
      lexer queueComments return >>= \ case
        L _ ITeof -> return []
        L _ t -> do
          psSpan <- toBufferSpan . prev_loc <$> getPState
          (L psSpan t :) <$> go
