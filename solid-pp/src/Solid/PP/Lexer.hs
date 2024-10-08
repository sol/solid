{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.PP.Lexer (
  Language
, LanguageFlag(..)
, Extension(..)
, showExtension
, applyLanguagePragmas

, LexerResult(..)
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
import           Data.List (stripPrefix)

import           Data.Map (Map)
import qualified Data.Map as Map

import           Solid.PP.SrcLoc

import           Lexer hiding (lexTokenStream)
import           GHC.Data.EnumSet (EnumSet)
import qualified GHC.Data.EnumSet as EnumSet
import           GHC.Data.FastString as FastString
import           GHC.Types.SourceText
import           GHC.Data.StringBuffer hiding (cur)
import           GHC.LanguageExtensions
import           GHC.Types.SrcLoc hiding (SrcLoc)
import           GHC.Utils.Error
import           GHC.Utils.Outputable (defaultSDocContext)
import           GHC.Driver.Errors.Types
import           GHC.Types.SourceError
import           GHC.Driver.Session hiding (language)
import qualified GHC.Parser.Header as ModuleHeader
import           GHC.Unit.Module.Warnings (emptyWarningCategorySet)

allExtensions :: Map String Extension
allExtensions = Map.fromList $ zip (map showExtension extensions) extensions
  where
    extensions :: [Extension]
    extensions = [minBound .. maxBound]

showExtension :: Extension -> String
showExtension = \ case
  Cpp -> "CPP"
  extension -> show extension

lookupExtension :: String -> Maybe Extension
lookupExtension = (`Map.lookup` allExtensions)

makeOpts :: Bool -> Bool -> EnumSet Extension -> ParserOpts
makeOpts keepComments interpretLinePragmas extensions = mkParserOpts extensions diagOpts allowedExtensions False keepComments keepComments interpretLinePragmas
  where
    allowedExtensions = Map.keys allExtensions ++ map ("No" <>) (Map.keys allExtensions)

    diagOpts = DiagOpts {
      diag_warning_flags = mempty
    , diag_fatal_warning_flags = mempty
    , diag_custom_warning_categories = emptyWarningCategorySet
    , diag_fatal_custom_warning_categories = emptyWarningCategorySet
    , diag_warn_is_error = False
    , diag_reverse_errors = False
    , diag_max_errors = Nothing
    , diag_ppr_ctx = defaultSDocContext
    }

data LanguageFlag = Enable Extension | Disable Extension

readLanguageFlag :: String -> Maybe LanguageFlag
readLanguageFlag input =
      Disable <$> (stripPrefix "-XNo" input >>= lookupExtension)
  <|> Enable  <$> (stripPrefix "-X"   input >>= lookupExtension)

applyLanguagePragmas :: EnumSet Extension -> FilePath -> StringBuffer -> EnumSet Extension
applyLanguagePragmas extensions src buffer = applyLanguageFlags extensions languageFlags
  where
    opts = makeOpts False True extensions
    (_, map unLoc -> options) = ModuleHeader.getOptions opts buffer src

    languageFlags :: [LanguageFlag]
    languageFlags = mapMaybe readLanguageFlag options

applyLanguageFlags :: EnumSet Extension -> [LanguageFlag] -> EnumSet Extension
applyLanguageFlags = foldl' (flip applyLanguageFlag)

applyLanguageFlag :: LanguageFlag -> EnumSet Extension -> EnumSet Extension
applyLanguageFlag = \ case
  Enable ext -> EnumSet.insert ext
  Disable ext -> EnumSet.delete ext

data LexerResult = LexerResult {
  tokens :: [WithBufferSpan Token]
, end :: SrcLoc
, errors :: String
} deriving Show

defaultExtensions :: Language -> EnumSet Extension
defaultExtensions language = EnumSet.fromList (languageExtensions (Just language))

pattern InfixProjection :: Token
pattern InfixProjection = ITproj False

pattern PrefixProjection :: Token
pattern PrefixProjection = ITproj True

change_any_projections_after_a_trailing_bang_to_infix :: [WithBufferSpan Token] -> [WithBufferSpan Token]
change_any_projections_after_a_trailing_bang_to_infix = fix $ \ rec -> \ case
  identifier@(L identifier_loc ITvarid {}) : L loc PrefixProjection : tokens | identifier_loc.end == loc.start -> identifier : L loc InfixProjection : rec tokens
  token : tokens -> token : rec tokens
  [] -> []

tokenize :: Language -> [LanguageFlag] -> FilePath -> Int -> Text -> Either String LexerResult
tokenize = tokenize_ False

tokenizeWithComments :: Language -> [LanguageFlag] -> FilePath -> Int -> Text -> Either String LexerResult
tokenizeWithComments = tokenize_ True

tokenize_ :: Bool -> Language -> [LanguageFlag] -> FilePath -> Int -> Text -> Either String LexerResult
tokenize_ keepComments language languageFlags src line input = do
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

    opts = makeOpts keepComments (not keepComments) (applyLanguagePragmas extensions src buffer)

    extensions :: EnumSet Extension
    extensions = applyLanguageFlags (defaultExtensions language) languageFlags

    loc :: RealSrcLoc
    loc = mkRealSrcLoc (mkFastString src) line 1

    buffer :: StringBuffer
    buffer = stringBufferFromByteString $ encodeUtf8 input

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
