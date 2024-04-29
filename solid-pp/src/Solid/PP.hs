{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
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
, ImplicitImport(..)
, implicitImport

, ImplicitImports
, implicitImports
#endif
) where

import           Prelude ()
import           Solid.PP.IO hiding (concatMap)

import qualified GHC.Data.FastString as GHC
import           Data.Coerce (coerce)
import           Control.Monad.Trans.Writer.CPS (execWriter, tell)
import           Data.Char
import           Data.Word
import qualified Data.Text as T
import qualified Data.ByteString.Short as SB
import           Data.Set (Set)
import qualified Data.Set as Set

import           Solid.PP.Builder (Builder)
import qualified Solid.PP.Builder as Builder
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
  , Enable LexicalNegation
  , Enable OverloadedRecordDot
  , Enable OverloadedStrings
  , Disable FieldSelectors
  ]

newtype ImplicitImport = ImplicitImport FastString
  deriving newtype (Eq, Show, IsString)

instance Ord ImplicitImport where
  compare = coerce GHC.uniqCompareFS

type ImplicitImports = Set ImplicitImport

wellKnownModules :: ImplicitImports
wellKnownModules = Set.fromList [
    "ByteString"
  , "Char"
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

toStringModule :: ImplicitImport
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
  org <- if src == cur then return input else readFile src
  preProcesses dst (InputFile src org) (InputFile cur $ addLinePragma input)
  where
    addLinePragma = (Builder.toText (formatLinePragma 1 src) <>)

linePragma :: SrcLoc -> Builder
linePragma loc = formatLinePragma loc.line loc.file

formatLinePragma :: Int -> FilePath -> Builder
formatLinePragma line src = "{-# LINE " <> Builder.int line <> " " <> Builder.show src <> " #-}\n"

preProcesses :: FilePath -> InputFile Original -> InputFile Current -> IO Result
preProcesses dst original current = case parseModule extensions original current of
  Left err -> return (Failure err)
  Right module_ -> withFile dst WriteMode $ \ h -> do
    edit (hPutStr h) current.contents (addImplicitImports module_ <> ppModule module_).build
    return Success

data Where = Before | After

addImplicitImports :: Module BufferSpan -> DList Edit
addImplicitImports module_ = case module_ of
  Module (ModuleHeader loc _ _) _ _ -> addImports After loc.endLoc
  Module NoModuleHeader (Import loc _ _ _ _ : _) _ -> addImports Before loc.startLoc
  Module NoModuleHeader [] (node : _) -> addImports Before node.start
  Module NoModuleHeader [] [] -> mempty
  where
    addImports where_ loc = case Set.null modules of
      True -> mempty
      False -> Edit.insert_ loc $ formatImports where_ modules <> linePragma loc

    modules :: ImplicitImports
    modules = Set.intersection (implicitImports module_) wellKnownModules

    formatImports :: Where -> ImplicitImports -> Builder
    formatImports = \ case
      Before -> Builder.unlines . map formatImport . Set.toList
      After -> ("\n" <>) . Builder.unlines . map formatImport . Set.toList

    formatImport :: ImplicitImport -> Builder
    formatImport (ImplicitImport m) = "import qualified " <> Builder.fastString m

implicitImport :: ModuleName () -> ImplicitImport
implicitImport = \ case
  (ModuleName () Nothing name) -> ImplicitImport name
  (ModuleName () (Just qualified) name) -> ImplicitImport $ mconcat [qualified, ".", name]

data EffectiveImport = EffectiveImport {
  _qualification :: Qualification
, _package :: PackageName
, _name :: ImplicitImport
, _as :: Maybe ImplicitImport
, _imports :: Imports
}

data Qualification = QualifiedImport | UnqualifiedImport

data Imports = NoImports | Imports

effectiveImport :: Import () -> EffectiveImport
effectiveImport = \ case
  Import () qualification (ImportName package name) as imports -> case qualification of
    Use -> effective $ maybe (useAsNameFromName name) (Just . implicitImport) as
    _   -> effective (implicitImport <$> as)
    where
      effective as_ = EffectiveImport (effectiveQualification qualification) package (implicitImport name) as_ (effectiveImports imports)

useAsNameFromName :: ModuleName () -> Maybe ImplicitImport
useAsNameFromName = \ case
  ModuleName () Nothing _ -> Nothing
  ModuleName () (Just _) as -> Just (ImplicitImport as)

effectiveImports :: ImportList a -> Imports
effectiveImports = \ case
  NoImportList -> NoImports
  ImportList _ -> Imports
  HidingList _ -> Imports

effectiveQualification :: ImportQualification -> Qualification
effectiveQualification = \ case
  Use -> QualifiedImport
  Qualified -> QualifiedImport
  QualifiedPost -> QualifiedImport
  Unqualified -> UnqualifiedImport

implicitImports :: Module BufferSpan-> ImplicitImports
implicitImports = ($ mempty) . fromModule . void
  where
    foreach :: Foldable sequence_of => (a -> ImplicitImports -> ImplicitImports) -> sequence_of a -> ImplicitImports -> ImplicitImports
    foreach f xs set = foldr f set xs

    fromModule :: Module () -> ImplicitImports -> ImplicitImports
    fromModule (Module header imports nodes) = foreach (fromImport . effectiveImport) imports . fromModuleHeader header . fromNodes nodes

    fromImport :: EffectiveImport -> ImplicitImports -> ImplicitImports
    fromImport = \ case
      EffectiveImport QualifiedImport NoPackageName _name Nothing NoImports -> id
      EffectiveImport _ _ name as _ -> Set.delete $ fromMaybe name as

    fromModuleHeader :: ModuleHeader () -> ImplicitImports -> ImplicitImports
    fromModuleHeader = \ case
      NoModuleHeader -> id
      ModuleHeader () name exports -> Set.delete (implicitImport name) . fromExportList exports

    fromExportList :: ExportList () -> ImplicitImports -> ImplicitImports
    fromExportList = \ case
      NoExportList -> id
      ExportList nodes -> foreach fromNodes nodes

    fromNodes :: [Node ()] -> ImplicitImports -> ImplicitImports
    fromNodes = foreach fromNode

    fromNode :: Node () -> ImplicitImports -> ImplicitImports
    fromNode = \ case
      Token () (ITqvarid (m, _)) -> Set.insert (ImplicitImport m)
      Token () (ITqconid (m, _)) -> Set.insert (ImplicitImport m)
      Token () (_ :: Token) -> id
      MethodDefinition method -> fromMethod method
      MethodChain subject methodCalls -> fromSubject subject . foreach fromMethodCall methodCalls

    fromMethod :: Method () -> ImplicitImports -> ImplicitImports
    fromMethod = \ case
      Method () (_ :: MethodName ()) context arguments subject result () () -> foreach fromType context . foreach fromType arguments . fromType subject . fromType result

    fromType :: Type () -> ImplicitImports -> ImplicitImports
    fromType = \ case
      TypeVariable _ -> id
      TypeName () (Just qualified) _name -> Set.insert (ImplicitImport qualified)
      TypeName () Nothing _name -> id
      TypeLiteral _ -> id
      Tuple ts -> foreach fromType ts
      ListOf t -> fromType t
      TypeApplication t1 t2 -> fromType t1 . fromType t2
      FunctionType t1 t2 -> fromType t1 . fromType t2
      TypeContext c t -> fromType c . fromType t

    fromSubject :: Subject () -> ImplicitImports -> ImplicitImports
    fromSubject = \ case
      LiteralString (Begin () (_ :: FastString) expression) -> Set.insert toStringModule . fromExpression expression
      LiteralString (Literal () (_ :: FastString)) -> id
      Bracketed (_ :: BracketStyle) () nodes -> foreach fromNodes nodes
      Name () (_ :: FastString) arguments -> fromArguments arguments
      QualifiedName () m (_ :: FastString) arguments -> Set.insert (ImplicitImport m) . fromArguments arguments

    fromMethodCall :: MethodCall () -> ImplicitImports -> ImplicitImports
    fromMethodCall (MethodCall () (_ :: FastString) arguments) = fromArguments arguments

    fromArguments :: Arguments () -> ImplicitImports -> ImplicitImports
    fromArguments = \ case
      NoArguments -> id
      Arguments () nodes -> foreach fromArgument nodes

    fromArgument :: Argument () -> ImplicitImports -> ImplicitImports
    fromArgument (Argument () nodes) = foreach fromNode nodes

    fromExpression :: Expression () -> ImplicitImports -> ImplicitImports
    fromExpression (Expression nodes end) = fromNodes nodes . case end of
      EndBegin () (_ :: FastString) expression -> fromExpression expression
      End () (_ :: FastString) -> id

desugarIdentifier :: Int -> Int -> FastString -> DList Edit
desugarIdentifier start end = maybe mempty replace_ . desugarIdentifierMaybe
  where
    replace_ :: Text -> DList Edit
    replace_ = singleton . Replace Nothing start (end - start)

desugarIdentifierMaybe :: FastString -> Maybe Text
desugarIdentifierMaybe identifier
  | lastChar == qmark || lastChar == bang = Just replacement
  | otherwise = Nothing
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

desugarQualifiedName :: BufferSpan -> FastString -> FastString -> DList Edit
desugarQualifiedName loc (succ . lengthFS -> offset) identifier = desugarIdentifier (loc.start + offset) loc.end identifier

unescapeString :: String -> String
unescapeString = go
  where
    go = \ case
      [] -> []
      '\\' : '{' : xs -> '{' : go xs
      x : xs -> x : go xs

unescapeStringLiteral :: BufferSpan -> FastString -> DList Edit
unescapeStringLiteral loc (unpackFS -> old)
  | new == old = mempty
  | otherwise = Edit.replaceText loc (pack new)
  where
    new = unescapeString old

ppModule :: Module BufferSpan -> DList Edit
ppModule (Module header imports nodes) = ppHeader header <> concatMap ppImport imports <> pp nodes

ppImport :: Import BufferSpan -> DList Edit
ppImport = \ case
  Import loc Use name as imports -> ppUseStatement loc name as <> ppImportList imports
  Import _ _ _ _ imports -> ppImportList imports
  where
    ppUseStatement :: BufferSpan -> ImportName BufferSpan -> Maybe (ModuleName BufferSpan) -> DList Edit
    ppUseStatement use (ImportName _ (ModuleName loc qualified name)) as = Edit.replaceText use "import" <> Edit.insert loc.endLoc case (qualified, as) of
      (Nothing, _) -> " qualified"
      (Just _, Nothing) -> " qualified as " <> Builder.fastString name
      _ -> " qualified"

    ppImportList :: ImportList BufferSpan -> DList Edit
    ppImportList = \ case
      NoImportList -> mempty
      ImportList names -> concatMap pp names
      HidingList names -> concatMap pp names

ppHeader :: ModuleHeader BufferSpan -> DList Edit
ppHeader = \ case
  NoModuleHeader -> mempty
  ModuleHeader _loc _name exports -> ppExportList exports

ppExportList :: ExportList BufferSpan -> DList Edit
ppExportList = \ case
  NoExportList -> mempty
  ExportList nodes -> concatMap pp nodes

pp :: [Node BufferSpan] -> DList Edit
pp = ppNodes
  where
    ppNodes :: Foldable sequence_of => sequence_of (Node BufferSpan) -> DList Edit
    ppNodes = concatMap ppNode

    ppNode :: Node BufferSpan -> DList Edit
    ppNode = \ case
      Token loc t -> ppToken loc t
      MethodDefinition method ->
           Edit.replace method.dot (formatMethod method)
        <> desugarIdentifier loc.start loc.end name
        <> Edit.replace method.definitionDot ""
        <> desugarIdentifier definitionName.start definitionName.end name
        where
          definitionName = method.definitionName
          MethodName loc name = method.name
      node@(MethodChain subject methodCalls) -> Edit.insertText node.start (T.replicate n "(") <> ppSubject subject <> concatMap ppMethodCall methodCalls
        where
          n :: Int
          n = length $ filter hasArguments methodCalls

          hasArguments :: MethodCall loc -> Bool
          hasArguments = \ case
            MethodCall _ _ NoArguments -> False
            MethodCall _ _ Arguments{} -> True

    formatMethod :: Method BufferSpan -> Builder
    formatMethod method =
         "instance " <> formatType 0 (foldr TypeContext instanceHead method.context)
      <> instanceBody
      <> linePragma method.dot.startLoc
      where
        instanceHead :: Type BufferSpan
        instanceHead = foldl1' TypeApplication [
            TypeName method.dot Nothing "HasField"
          , TypeLiteral (show name)
          , method.subject
          , foldr FunctionType method.result method.arguments
          ]

        name :: Text
        name = desugarMethodName method.name

        instanceBody :: Builder
        instanceBody = case length method.arguments of
          0 -> " where getField = " <> Builder.fromText name <> "\n"
          n -> " where getField _subject" <> args <> " = " <> Builder.fromText name <> args <> " _subject\n"
            where
              args :: Builder
              args = mconcat (take n ids)

              ids :: [Builder]
              ids = map (fromString . (" _" ++)) names
                where
                  names :: [String]
                  names = [x : xs | xs <- "" : names, x <- ['a' .. 'z']]

    desugarMethodName :: MethodName loc -> Text
    desugarMethodName (MethodName _ name) = fromMaybe (pack $ unpackFS name) $ desugarIdentifierMaybe name

    formatType :: Int -> Type BufferSpan -> Builder
    formatType p = \ case
      TypeVariable name -> Builder.fastString name
      TypeName loc Nothing t -> Edit.formatColumnPragma loc.startColumn <> Builder.fastString t
      TypeName loc (Just qualified) t -> Edit.formatColumnPragma loc.startColumn <> Builder.fastString qualified <> "." <> Builder.fastString t
      TypeLiteral literal -> fromString literal
      Tuple ts -> "(" <> Builder.join ", " (map (formatType 0) ts) <> ")"
      ListOf t -> "[" <> formatType 0 t <> "]"
      TypeApplication t1 t2 -> infixL 3 t1 " " t2
      FunctionType t1 t2 -> infixR 2 t1 " -> " t2
      TypeContext c t -> infixR 1 c " => " t
      where
        infixL :: Int -> Type BufferSpan -> Builder -> Type BufferSpan -> Builder
        infixL n t1 sep t2 = parens n $ formatType n t1 <> sep <> formatType (succ n) t2

        infixR :: Int -> Type BufferSpan -> Builder -> Type BufferSpan -> Builder
        infixR n t1 sep t2 = parens n $ formatType (succ n) t1 <> sep <> formatType n t2

        parens n term
          | p > n = "(" <> term <> ")"
          | otherwise = term

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
          Arguments {} -> Edit.insertText start "("

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
    ppArgument (Argument loc nodes) = Edit.replaceText loc ")(" <> ppNodes nodes

    ppToken :: BufferSpan -> Token -> DList Edit
    ppToken loc = \ case
      ITvarid identifier -> desugarIdentifier loc.start loc.end identifier
      ITqvarid (module_, name) -> desugarQualifiedName loc module_ name
      _ -> mempty

    ppLiteralString :: LiteralString BufferSpan -> DList Edit
    ppLiteralString = \ case
      Literal loc src -> unescapeStringLiteral loc src
      Begin loc src expression -> Edit.replace loc (lambdaAbstract expression <> beginInterpolation src) <> ppExpression 1 expression

    ppExpression :: Int -> Expression BufferSpan -> DList Edit
    ppExpression n = \ case
      Expression [] end -> Edit.insert end.loc.startLoc (abstractionParam n) <> ppEnd (succ n) end
      Expression nodes end -> ppNodes nodes <> ppEnd n end

    ppEnd :: Int -> End BufferSpan -> DList Edit
    ppEnd n = \ case
      End loc src -> endInterpolation loc src
      EndBegin loc src expression -> Edit.replace loc (endBeginInterpolation src) <> ppExpression n expression

    unescape :: String -> (Maybe Builder)
    unescape (init . drop 1 -> string)
      | null string = Nothing
      | otherwise = Just (fromString $ unescapeString string)

    beginInterpolation :: FastString -> Builder
    beginInterpolation src = literal <> "Solid.ToString.toString ("
      where
        literal = case unescape (unpackFS src) of
          Nothing -> ""
          Just string -> "\"" <> string <> "\" <> "

    endInterpolation :: BufferSpan -> FastString -> DList Edit
    endInterpolation loc src = Edit.replace_ loc end <> close_paren loc.endLoc
      where
        end :: Builder
        end = case unescape (unpackFS src) of
          Nothing -> ")"
          Just string -> ") <> \"" <> string <> "\""

    endBeginInterpolation :: FastString -> Builder
    endBeginInterpolation src = ") <> " <> beginInterpolation src

    close_paren :: SrcLoc -> DList Edit
    close_paren = singleton . Edit.insertClosingParen

lambdaAbstract :: Expression BufferSpan -> Builder
lambdaAbstract = lambda . countAbstractions
  where
    lambda :: Int -> Builder
    lambda n
      | n == 0 = "("
      | otherwise = "(\\" <> params <> " -> "
      where
        params :: Builder
        params = Builder.concatMap formatParam [1..n]

    formatParam :: Int -> Builder
    formatParam n = " " <> abstractionParam n

    countAbstractions :: Expression BufferSpan -> Int
    countAbstractions = onExpression
      where
        onExpression :: Expression BufferSpan -> Int
        onExpression = \ case
          Expression nodes end -> onEnd end + case nodes of
            [] -> 1
            _ -> 0

        onEnd :: End BufferSpan -> Int
        onEnd = \ case
          End _ _ -> 0
          EndBegin _ _ expression -> onExpression expression

abstractionParam :: Int -> Builder
abstractionParam n = Builder.char '_' <> Builder.int n
