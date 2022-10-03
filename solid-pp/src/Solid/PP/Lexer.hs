{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.PP.Lexer (
  Extension(..)
, getExtensions

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
import           Data.List (stripPrefix)
import           Text.Read

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
import           GHC.Parser.Header

supportedExtensions :: [Extension]
supportedExtensions = [minBound .. maxBound]

makeOpts :: [Extension] -> ParserOpts
makeOpts extensions = mkParserOpts exts diagOpts (map show supportedExtensions) False True True True
  where
    exts = EnumSet.fromList $ languageExtensions (Just GHC2021) ++ extensions

    diagOpts = DiagOpts {
      diag_warning_flags       = mempty
    , diag_fatal_warning_flags = mempty
    , diag_warn_is_error       = False
    , diag_reverse_errors      = False
    , diag_max_errors          = Nothing
    , diag_ppr_ctx             = defaultSDocContext
    }

deriving instance Read Extension

getExtensions :: [Extension] -> FilePath -> StringBuffer -> [Extension]
getExtensions extensions src buffer = extensions ++ mapMaybe (readExtension . unLoc) options
  where
    opts = makeOpts extensions
    (_, options) = getOptions opts buffer src

    readExtension :: String -> Maybe Extension
    readExtension = stripPrefix "-X" >=> readMaybe

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
    opts = makeOpts (getExtensions extensions src buffer)

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
