{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.PP.Lexer (
  Extension(..)
, applyLanguagePragmas

, LexerResult(..)
, Token(..)
, SourceError
, tokenize

, StringBuffer
, stringToStringBuffer

, module FastString
, module GHC.Types.SourceText

, module Solid.PP.SrcLoc
) where

import           Prelude ()
import           Solid.PP.IO

import           Data.List (stripPrefix)
import qualified Data.Text.Encoding as T

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
import           GHC.Types.SrcLoc
import           GHC.Utils.Error
import           GHC.Utils.Outputable (defaultSDocContext)
import           GHC.Driver.Errors.Types
import           GHC.Types.SourceError
import           GHC.Driver.Session
import qualified GHC.Parser.Header as ModuleHeader

instance HasField "toText" FastString Text where
  getField = T.decodeUtf8Lenient . bytesFS

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

makeOpts :: EnumSet Extension -> ParserOpts
makeOpts extensions = mkParserOpts extensions diagOpts allowedExtensions False True True True
  where
    allowedExtensions = Map.keys allExtensions ++ map ("No" <>) (Map.keys allExtensions)

    diagOpts = DiagOpts {
      diag_warning_flags       = mempty
    , diag_fatal_warning_flags = mempty
    , diag_warn_is_error       = False
    , diag_reverse_errors      = False
    , diag_max_errors          = Nothing
    , diag_ppr_ctx             = defaultSDocContext
    }

data LanguageFlag = Enable Extension | Disable Extension

readLanguageFlag :: String -> Maybe LanguageFlag
readLanguageFlag input =
      Disable <$> (stripPrefix "-XNo" input >>= lookupExtension)
  <|> Enable  <$> (stripPrefix "-X"   input >>= lookupExtension)

applyLanguagePragmas :: [Extension] -> FilePath -> StringBuffer -> EnumSet Extension
applyLanguagePragmas (EnumSet.fromList -> extensions) src buffer = foldl' (flip applyLanguageFlag) extensions languageFlags
  where
    opts = makeOpts extensions
    (_, map unLoc -> options) = ModuleHeader.getOptions opts buffer src

    applyLanguageFlag :: LanguageFlag -> EnumSet Extension -> EnumSet Extension
    applyLanguageFlag = \ case
      Enable ext -> EnumSet.insert ext
      Disable ext -> EnumSet.delete ext

    languageFlags :: [LanguageFlag]
    languageFlags = mapMaybe readLanguageFlag options

data LexerResult = LexerResult {
  tokens :: [WithBufferSpan Token]
, end :: RealSrcLoc
, errors :: String
}

tokenize :: [Extension] -> FilePath -> Text -> Either String LexerResult
tokenize extensions src input = do
  case lexTokenStream opts buffer loc of
    POk state a -> return LexerResult {
      tokens = a
    , end = psRealLoc state.loc
    , errors = errors state
    }

    PFailed state -> Left (errors state)
  where
    errors :: PState -> String
    errors = show . getErrors

    opts = makeOpts (applyLanguagePragmas (languageExtensions (Just GHC2021) ++ extensions ) src buffer)

    loc :: RealSrcLoc
    loc = mkRealSrcLoc (mkFastString src) 1 1

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
