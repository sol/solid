{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Solid.PP.Lexer (
  Extension(..)
, applyLanguagePragmas

, Token(..)
, SourceError
, tokenize
, tokenizeWithErrors

, StringBuffer
, stringToStringBuffer

, FastString
, unpackFS

, PsLocated
, GenLocated(..)
, unLoc
, PsSpan(..)
, BufSpan(..)
, BufPos(..)
) where

import           Prelude ()
import           Solid.PP.IO

import           Data.Maybe
import           Data.List (stripPrefix, foldl')

import           Data.Map (Map)
import qualified Data.Map as Map

import           GHC.Data.EnumSet (EnumSet)
import qualified GHC.Data.EnumSet as EnumSet
import           GHC.Data.FastString
import           GHC.Data.StringBuffer hiding (cur)
import           GHC.LanguageExtensions
import           GHC.Parser.Lexer hiding (lexTokenStream)
import           GHC.Types.SrcLoc
import           GHC.Utils.Error
import           GHC.Utils.Outputable (defaultSDocContext)
import           GHC.Driver.Errors.Types
import           GHC.Types.SourceError
import           GHC.Driver.Session
import qualified GHC.Parser.Header as ModuleHeader

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

tokenize :: [Extension] -> FilePath -> Text -> IO [PsLocated Token]
tokenize extensions src = fmap snd . tokenizeWithErrors extensions src

tokenizeWithErrors :: [Extension] -> FilePath -> Text -> IO (SourceError, [PsLocated Token])
tokenizeWithErrors extensions src input = do
  case lexTokenStream opts buffer loc of
    POk state a -> do
      return (getErrors state, a)
    PFailed state -> do
      throwIO (getErrors state)
  where
    opts = makeOpts (applyLanguagePragmas (languageExtensions (Just GHC2021) ++ extensions ) src buffer)

    loc :: RealSrcLoc
    loc = mkRealSrcLoc (mkFastString src) 1 1

    buffer :: StringBuffer
    buffer = stringBufferFromByteString $ encodeUtf8 input

    getErrors = mkSrcErr . fmap GhcPsMessage . getPsErrorMessages

lexTokenStream :: ParserOpts -> StringBuffer -> RealSrcLoc -> ParseResult [PsLocated Token]
lexTokenStream opts buf loc = unP go (initParserState opts buf loc)
  where
    queueComments = False

    go :: P [PsLocated Token]
    go = do
      lexer queueComments return >>= \ case
        L _ ITeof -> return []
        L _ t -> do
          psSpan <- last_loc <$> getPState
          (L psSpan t :) <$> go
