{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
module Solid.PP.Lexer.Extensions (
  Language(..)
, showLanguageFlag

, ExtensionFlag
, OnOff(..)
, Extension(..)
, showExtensionFlag

, makeOpts

, StringBuffer
, EnumSet
, extensionsFromModuleHeader

#ifdef TEST
, parseLanguageFlag
, parseExtensionFlag
, extensionName
, knownLanguagePragmas
#endif
) where

import           Prelude ()
import           Solid.PP.IO

import           Data.Either
import           Data.List (stripPrefix)

import           Data.Map (Map)
import qualified Data.Map as Map

import           Lexer hiding (lexTokenStream)
import           GHC.Data.EnumSet (EnumSet)
import qualified GHC.Data.EnumSet as EnumSet
import           GHC.Data.StringBuffer
import           GHC.LanguageExtensions
import           GHC.Utils.Error
import           GHC.Driver.Session hiding (language, impliedXFlags)
import qualified GHC.Driver.Session as GHC
import qualified GHC.Parser.Header as ModuleHeader
import           GHC.Platform

import           Solid.PP.SrcLoc

showLanguageFlag :: Language -> String
showLanguageFlag = mappend "-X" . show

parseLanguageFlag :: String -> Maybe Language
parseLanguageFlag = \ case
  "-XHaskell98" -> Just Haskell98
  "-XHaskell2010" -> Just Haskell2010
  "-XGHC2021" -> Just GHC2021
  "-XGHC2024" -> Just GHC2024
  _ -> Nothing

type ExtensionFlag = OnOff Extension

showExtensionFlag :: ExtensionFlag -> String
showExtensionFlag = \ case
  On extension  -> "-X"   <> extensionName extension
  Off extension -> "-XNo" <> extensionName extension

#if __GLASGOW_HASKELL__ > 910
import GHC.Driver.Flags (extensionName)
#else
extensionName :: Extension -> String
extensionName = \ case
  Cpp -> "CPP"
  extension -> show extension
#endif

parseExtensionFlag :: String -> Maybe ExtensionFlag
parseExtensionFlag input =
      Off <$> (stripPrefix "-XNo" input >>= lookupExtension)
  <|> On  <$> (stripPrefix "-X"   input >>= lookupExtension)

lookupExtension :: String -> Maybe Extension
lookupExtension = (`Map.lookup` extensionsMap)

extensionsMap :: Map String Extension
extensionsMap = Map.fromList $ map (extensionName &&& id) [minBound .. maxBound]

makeOpts :: Bool -> Bool -> EnumSet Extension -> ParserOpts
makeOpts keepComments interpretLinePragmas extensions = mkParserOpts extensions emptyDiagOpts knownLanguagePragmas False keepComments keepComments interpretLinePragmas

knownLanguagePragmas :: [String]
knownLanguagePragmas = supportedLanguagesAndExtensions $ ArchOS ArchWasm32 OSLinux

extensionsFromModuleHeader :: Maybe Language -> [ExtensionFlag] -> FilePath -> StringBuffer -> EnumSet Extension
extensionsFromModuleHeader defaultLanguage defaultExtensions src buffer = flattenExtensionFlags language extensions
  where
    language :: Maybe Language
    language = (listToMaybe . reverse . lefts) pragmas <|> defaultLanguage

    extensions :: [ExtensionFlag]
    extensions = defaultExtensions ++ rights pragmas

    pragmas :: [Either Language ExtensionFlag]
    pragmas = extractLanguagePragmas src buffer

extractLanguagePragmas :: FilePath -> StringBuffer -> [Either Language ExtensionFlag]
extractLanguagePragmas src buffer = mapMaybe parseLanguageOption options
  where
    parseLanguageOption :: String -> Maybe (Either Language ExtensionFlag)
    parseLanguageOption = (<|>) <$> (fmap Left . parseLanguageFlag) <*> (fmap Right . parseExtensionFlag)

    options :: [String]
    options = map unLoc . snd $ ModuleHeader.getOptions opts buffer src

    opts :: ParserOpts
    opts = makeOpts False True mempty

flattenExtensionFlags :: Maybe Language -> [ExtensionFlag] -> EnumSet Extension
flattenExtensionFlags language = foldl' (flip setExtensionFlag) defaults
  where
    defaults :: EnumSet Extension
    defaults = case language of
      Nothing -> mempty
      Just _ -> EnumSet.fromList (languageExtensions language)

    setExtensionFlag :: ExtensionFlag -> EnumSet Extension -> EnumSet Extension
    setExtensionFlag = \ case
      On extension -> enableImpliedExtensions extension . EnumSet.insert extension
      Off extension -> EnumSet.delete extension

    enableImpliedExtensions :: Extension -> EnumSet Extension -> EnumSet Extension
    enableImpliedExtensions = (`Map.lookup` impliedExtensions) >>> fromMaybe id

    impliedExtensions :: Map Extension (EnumSet Extension -> EnumSet Extension)
    impliedExtensions = Map.fromListWith (.) $ map (fmap setExtensionFlag) impliedXFlags

impliedXFlags :: [(Extension, ExtensionFlag)]
impliedXFlags = map f GHC.impliedXFlags
  where
    f :: (Extension, Bool, Extension) -> (Extension, ExtensionFlag)
    f (extension, on, implied) = case on of
      True -> (extension, On implied)
      False -> (extension, Off implied)
