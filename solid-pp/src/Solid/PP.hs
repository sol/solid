{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
module Solid.PP (
  main
, desugarExpression

, Result(..)
, run

, Language
, language

, LanguageFlag(..)
, Extension(..)
, showExtension
, extensions

#ifdef TEST
, usedModules
#endif
) where

import           Prelude ()
import           Solid.PP.IO hiding (concatMap)

import           Control.Monad.Trans.Writer.CPS (execWriter, tell)
import           Data.Char
import           Data.Word
import qualified Data.Text as T
import qualified Data.ByteString.Short as SB
import           Data.Set (Set)
import qualified Data.Set as Set

import           Solid.PP.DList
import           Solid.PP.Edit (Edit(..), edit)
import qualified Solid.PP.Edit as Edit
import           Solid.PP.Lexer
import           Solid.PP.Parser

extensions :: [LanguageFlag]
extensions = [
    Enable DataKinds
  , Enable DeriveAnyClass
  , Enable DuplicateRecordFields
  , Enable LambdaCase
  , Enable OverloadedRecordDot
  , Enable OverloadedStrings
  , Disable FieldSelectors
  ]

wellKnownModules :: Set ModuleName
wellKnownModules = Set.fromList [
    "ByteString"
  , "Directory"
  , "Either"
  , "Env"
  , "Exception"
  , "FilePath"
  , "IO"
  , "List"
  , "Maybe"
  , "Platform"
  , "Prelude"
  , "Process"
  , "Process.Config"
  , "Process.Config.STDERR"
  , "Process.Config.STDIN"
  , "Process.Config.STDOUT"
  , "String"
  , "Temp"
  , "Word8"
  , toStringModule
  ]

toStringModule :: ModuleName
toStringModule = "Solid.ToString"

data Result = Failure String | Success
  deriving (Eq, Show)

desugarExpression :: FilePath -> Int -> Text -> Either String Text
desugarExpression src line input = execWriter . edit tell input . (.build) . pp <$> parseExpression extensions src line input

main :: String -> String -> String -> IO ()
main src cur dst = run src cur dst >>= \ case
  Failure err -> die $ \ _ -> err
  Success -> return ()

run :: FilePath -> FilePath -> FilePath -> IO Result
run src cur dst = do
  input <- readFile cur
  preProcesses src dst $ addLinePragma input
  where
    addLinePragma = (linePragma 1 src <>)

linePragma :: Int -> FilePath -> Text
linePragma line src = pack $ "{-# LINE " <> show line <> " " <> show src <> " #-}\n"

preProcesses :: FilePath -> FilePath -> Text -> IO Result
preProcesses src dst input = case parseModule extensions src 1 input of
  Left err -> return (Failure err)
  Right module_ -> withFile dst WriteMode $ \ h -> do
    edit (hPutStr h) input $ addImplicitImports module_ (ppModule module_)
    return Success

data Where = Before | After

addImplicitImports :: Module BufferSpan -> [Edit] -> [Edit]
addImplicitImports module_ = case module_ of
  Module NoModuleHeader [] -> id
  Module NoModuleHeader (node : _) -> addImports Before node.start modules
  Module (ModuleHeader loc name _) _ -> addImports After loc.endLoc (Set.delete name modules)
  where
    addImports where_ loc imports = case Set.null imports of
      True -> id
      False -> (:) $ insert loc.offset $ formatImports where_ imports <> linePragma loc.line loc.file

    modules :: Set ModuleName
    modules = Set.intersection (usedModules module_) wellKnownModules

    formatImports :: Where -> Set ModuleName -> Text
    formatImports = \ case
      Before -> T.unlines . map formatImport . Set.toList
      After -> ("\n" <>) . T.unlines . map formatImport . Set.toList

    formatImport :: ModuleName -> Text
    formatImport (ModuleName m) = "import qualified " <> m.toText

    insert :: Int -> Text -> Edit
    insert offset = Replace Nothing offset 0

usedModules :: Module BufferSpan-> Set ModuleName
usedModules = Set.fromList . (.build) . fromModule . void
  where
    fromModule :: Module () -> DList ModuleName
    fromModule (Module header nodes) = fromModuleHeader header <> fromNodes nodes

    fromModuleHeader :: ModuleHeader () -> DList ModuleName
    fromModuleHeader = \ case
      NoModuleHeader -> mempty
      ModuleHeader () (_ :: ModuleName) exports -> fromExportList exports

    fromExportList :: ExportList () -> DList ModuleName
    fromExportList = \ case
      NoExportList -> mempty
      ExportList nodes -> concatMap fromNodes nodes

    fromNodes :: [NodeWith ()] -> DList ModuleName
    fromNodes = concatMap fromNode

    fromNode :: NodeWith () -> DList ModuleName
    fromNode = \ case
      Token () (ITqvarid (m, _)) -> singleton (ModuleName m)
      Token () (_ :: Token) -> mempty
      MethodChain subject methodCalls -> fromSubject subject <> concatMap fromMethodCall methodCalls

    fromSubject :: Subject () -> DList ModuleName
    fromSubject = \ case
      LiteralString (Begin () (_ :: String) expression) -> singleton toStringModule <> fromExpression expression
      LiteralString (Literal () (_ :: String)) -> mempty
      Bracketed (_ :: BracketStyle) () nodes -> concatMap fromNodes nodes
      Name () (_ :: FastString) arguments -> fromArguments arguments
      QualifiedName () m (_ :: FastString) arguments -> singleton (ModuleName m) <> fromArguments arguments

    fromMethodCall :: MethodCall () -> DList ModuleName
    fromMethodCall (MethodCall () (_ :: FastString) arguments) = fromArguments arguments

    fromArguments :: Arguments () -> DList ModuleName
    fromArguments = \ case
      NoArguments -> mempty
      Arguments () nodes -> concatMap fromArgument nodes

    fromArgument :: Argument () -> DList ModuleName
    fromArgument (Argument () nodes) = concatMap fromNode nodes

    fromExpression :: ExpressionWith () -> DList ModuleName
    fromExpression (Expression nodes end) = fromNodes nodes <> case end of
      EndBegin () (_ :: String) expression -> fromExpression expression
      End () (_ :: String) -> mempty

desugarIdentifier :: Int -> Int -> FastString -> DList Edit
desugarIdentifier start end identifier
  | lastChar == qmark || lastChar == bang = replace replacement
  | otherwise = mempty
  where
    lastChar :: Word8
    lastChar = SB.last $ fastStringToShortByteString identifier

    qmark :: Word8
    qmark = fromIntegral $ ord '?'

    bang :: Word8
    bang = fromIntegral $ ord '!'

    replacement :: Text
    replacement = T.pack $ map replaceChar $ unpackFS identifier

    replaceChar :: Char -> Char
    replaceChar c = case c of
      '!' -> 'ᴉ'
      '?' -> 'ʔ'
      _ -> c

    replace :: Text -> DList Edit
    replace = singleton . Replace Nothing start (end - start)

desugarQualifiedName :: BufferSpan -> FastString -> FastString -> DList Edit
desugarQualifiedName loc (succ . lengthFS -> offset) identifier = desugarIdentifier (loc.start + offset) loc.end identifier

data WithColumnPragma = WithColumnPragma | WithoutColumnPragma

replaceBufferSpan :: WithColumnPragma -> BufferSpan -> String -> Edit
replaceBufferSpan with_pragma loc = Replace pragma loc.start loc.length . pack
  where
    pragma :: Maybe Int
    pragma = case with_pragma of
      WithColumnPragma -> Just loc.startColumn
      WithoutColumnPragma -> Nothing

unescapeString :: String -> String
unescapeString = go
  where
    go = \ case
      [] -> []
      '\\' : '{' : xs -> '{' : go xs
      x : xs -> x : go xs

unescapeStringLiteral :: BufferSpan -> String -> DList Edit
unescapeStringLiteral loc old
  | new == old = mempty
  | otherwise = singleton $ replaceBufferSpan WithColumnPragma loc new
  where
    new = unescapeString old

ppModule :: Module BufferSpan -> [Edit]
ppModule (Module header nodes) = (ppHeader header <> pp nodes).build

ppHeader :: ModuleHeader BufferSpan -> DList Edit
ppHeader = \ case
  NoModuleHeader -> mempty
  ModuleHeader _loc _name exports -> ppExportList exports

ppExportList :: ExportList BufferSpan -> DList Edit
ppExportList = \ case
  NoExportList -> mempty
  ExportList nodes -> concatMap pp nodes

pp :: [Node] -> DList Edit
pp = ppNodes
  where
    ppNodes :: Foldable sequence_of => sequence_of Node -> DList Edit
    ppNodes = concatMap ppNode

    ppNode :: Node -> DList Edit
    ppNode = \ case
      Token loc t -> ppToken loc t
      node@(MethodChain subject methodCalls) -> insert node.start (replicate n '(') <> ppSubject subject <> concatMap ppMethodCall methodCalls
        where
          n :: Int
          n = length $ filter hasArguments methodCalls

          hasArguments :: MethodCall loc -> Bool
          hasArguments = \ case
            MethodCall _ _ NoArguments -> False
            MethodCall _ _ Arguments{} -> True

    ppSubject :: Subject BufferSpan -> DList Edit
    ppSubject = \ case
      LiteralString string -> ppLiteralString string
      Bracketed (_ :: BracketStyle) (_ :: BufferSpan) nodes -> concatMap ppNodes nodes
      Name start identifier arguments -> ppArguments start.startLoc (desugarIdentifier start.start start.end identifier) arguments
      QualifiedName start module_ identifier arguments -> ppArguments start.startLoc (desugarQualifiedName start module_ identifier) arguments

    ppArguments :: SrcLoc -> DList Edit -> Arguments BufferSpan -> DList Edit
    ppArguments start subject arguments = open <> subject <> ppCloseArguments arguments
      where
        open = case arguments of
          NoArguments -> mempty
          Arguments {} -> insert start "("

    ppMethodCall :: MethodCall BufferSpan -> DList Edit
    ppMethodCall (MethodCall loc name arguments) = desugarIdentifier loc.start loc.end name <> ppCloseArguments arguments

    ppCloseArguments :: Arguments BufferSpan -> DList Edit
    ppCloseArguments NoArguments = mempty
    ppCloseArguments (Arguments end nodes) = arguments <> close_paren end.endLoc
      where
        arguments = case nodes of
          [] -> mempty
          Argument _ arg : args -> ppNodes arg <> concatMap ppArgument args

    ppArgument :: Argument BufferSpan -> DList Edit
    ppArgument (Argument loc nodes) = replace loc ")(" <> ppNodes nodes

    ppToken :: BufferSpan -> Token -> DList Edit
    ppToken loc = \ case
      ITvarid identifier -> desugarIdentifier loc.start loc.end identifier
      ITqvarid (module_, name) -> desugarQualifiedName loc module_ name
      _ -> mempty

    ppLiteralString :: LiteralString BufferSpan -> DList Edit
    ppLiteralString = \ case
      Literal loc src -> unescapeStringLiteral loc src
      Begin loc src expression -> replace loc (lambdaAbstract expression <> beginInterpolation src).build <> ppExpression 1 expression

    ppExpression :: Int -> Expression -> DList Edit
    ppExpression n = \ case
      Expression [] end -> insert end.loc.startLoc (abstractionParam n "") <> ppEnd (succ n) end
      Expression nodes end -> ppNodes nodes <> ppEnd n end

    ppEnd :: Int -> End BufferSpan -> DList Edit
    ppEnd n = \ case
      End loc src -> endInterpolation loc src
      EndBegin loc src expression -> replace loc (endBeginInterpolation src).build <> ppExpression n expression

    unescape :: String -> (Maybe DString)
    unescape (init . tail -> string)
      | null string = Nothing
      | otherwise = Just (fromString $ unescapeString string)

    beginInterpolation :: String -> DString
    beginInterpolation src = literal <> "Solid.ToString.toString ("
      where
        literal = case unescape src of
          Nothing -> ""
          Just string -> "\"" <> string <> "\" <> "

    endInterpolation :: BufferSpan -> String -> DList Edit
    endInterpolation loc src = singleton (replaceBufferSpan WithoutColumnPragma loc end) <> close_paren loc.endLoc
      where
        end :: String
        end = case unescape src of
          Nothing -> ")"
          Just string -> (") <> \"" <> string <> "\"").build

    endBeginInterpolation :: String -> DString
    endBeginInterpolation src = ") <> " <> beginInterpolation src

    replace :: BufferSpan -> String -> DList Edit
    replace loc = singleton . replaceBufferSpan WithColumnPragma loc

    insert :: SrcLoc -> String -> DList Edit
    insert loc = singleton . Replace (Just loc.column) loc.offset 0 . pack

    close_paren :: SrcLoc -> DList Edit
    close_paren = singleton . Edit.insertClosingParen

lambdaAbstract :: Expression -> DString
lambdaAbstract = lambda . countAbstractions
  where
    lambda :: Int -> DString
    lambda n
      | n == 0 = "("
      | otherwise = "(\\" <> params <> " -> "
      where
        params :: DString
        params = concatMap formatParam [1..n]

    formatParam :: Int -> DString
    formatParam n = DList $ (' ' :) . abstractionParam n

    countAbstractions :: Expression -> Int
    countAbstractions = onExpression
      where
        onExpression :: Expression -> Int
        onExpression = \ case
          Expression nodes end -> onEnd end + case nodes of
            [] -> 1
            _ -> 0

        onEnd :: End BufferSpan -> Int
        onEnd = \ case
          End _ _ -> 0
          EndBegin _ _ expression -> onExpression expression

abstractionParam :: Int -> ShowS
abstractionParam n = ('_' :) . shows n
