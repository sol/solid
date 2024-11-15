{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.PP.Parser (
  Original
, Current
, InputFile(..)
, parseModule
, parseExpression

, Module(..)
, ModuleHeader(..)
, ModuleName(..)
, ExportList(..)
, Import(..)
, ImportQualification(..)
, ImportName(..)
, PackageName(..)
, ImportList(..)

, Method(..)
, MethodName(..)
, WithStackTrace(..)
, Type(..)

, Subject(..)
, Arguments(..)
, Argument(..)
, MethodCall(..)

, Node(..)
, LiteralString(..)
, Expression(..)
, End(..)
) where

import           Prelude ()
import           Solid.PP.IO hiding (try, error, some, many)

import           Data.Foldable1 (fold1)
import           Data.List hiding (lines)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Text.Megaparsec as P
import           Text.Megaparsec hiding (Token, token, tokens, parse, parseTest, some)
import           Control.Applicative.Combinators.NonEmpty
import           Control.Monad.Combinators.Expr

import           Solid.PP.Lexer hiding (Token, LexerResult(..), Extension(..))
import qualified Solid.PP.Lexer as Lexer

import qualified GHC.Types.Basic as GHC
import qualified GHC.Hs.DocString as GHC
import qualified GHC.Types.SrcLoc as GHC
import qualified GHC.Data.FastString as GHC
import           GHC.Parser.Annotation (HasE(..), IsUnicodeSyntax(..))

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

  tokensLength :: Proxy TokenStream -> NonEmpty Token -> Int
  tokensLength Proxy = (.length) . fold1 . fmap getLoc

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
showToken t = case tokenName t of
  Nothing -> show t
  Just name -> name

instance ShowErrorComponent Error where
  showErrorComponent = \ case
    UnterminatedStringInterpolation -> "unterminated string interpolation"
    UnterminatedMethodCall -> "unterminated method call"
    InvalidMethodArity -> "invalid method type (arity must be at least 1)"
    ExpectingMethodName expected (Just actual) -> "unexpected method name " <> show actual <> ", expecting " <> show expected
    ExpectingMethodName expected Nothing -> "expecting method name " <> show expected

data Error =
    UnterminatedStringInterpolation
  | UnterminatedMethodCall
  | InvalidMethodArity
  | ExpectingMethodName FastString (Maybe FastString)
  deriving (Eq, Show, Ord)

error :: Error -> Parser a
error = fancyFailure . Set.singleton . ErrorCustom

indentationError :: Pos -> BufferSpan -> Parser a
indentationError expected actual = fancyFailure $ Set.singleton $ ErrorIndentation EQ expected (mkPos actual.startColumn)

data Original
data Current

data InputFile a = InputFile {
  name :: FilePath
, contents :: Text
} deriving (Eq, Show)

parseModule :: Language -> [LanguageFlag] -> InputFile Original -> InputFile Current -> Either String (Module BufferSpan)
parseModule language extensions = parse pModule language extensions 1

parseExpression :: Language -> [LanguageFlag] -> FilePath -> Int -> Text -> Either String [Node BufferSpan]
parseExpression language extensions src line input = parse pModuleBody language extensions line (InputFile src input) (InputFile src input)

parse :: Parser a -> Language -> [LanguageFlag] -> Int -> InputFile Original -> InputFile Current -> Either String a
parse parser language extensions line original current = do
  result <- tokenize language extensions original.name line current.contents
  let
    stream :: TokenStream
    stream = TokenStream original.contents result.tokens
  case P.parse parser original.name stream of
    Left err -> Left $ errorBundlePrettyForGhcPreProcessors err
    Right r -> Right r

token :: (Token -> Maybe a) -> Parser a
token = (`P.token` mempty)

require :: Lexer.Token -> Parser BufferSpan
require expected = token \ case
  L loc t | t == expected -> Just loc
  _ -> Nothing

pModule :: Parser (Module BufferSpan)
pModule = Module <$> pModuleHeader <*> many (pUse <|> pImport) <*> pModuleBody

pModuleHeader :: Parser (ModuleHeader BufferSpan)
pModuleHeader = moduleHeader <|> pure NoModuleHeader
  where
    moduleHeader = do
      start <- require ITmodule
      name <- pModuleName
      exports <- pExportList
      end <- require ITwhere <* require ITvocurly
      return $ ModuleHeader (start.merge end) name exports

pModuleName :: Parser (ModuleName BufferSpan)
pModuleName = token \ case
  L loc (ITconid name) -> Just $ ModuleName loc Nothing name
  L loc (ITqconid (qualified, name)) -> Just $ ModuleName loc (Just qualified) name
  _ -> Nothing

pExportList :: Parser (ExportList BufferSpan)
pExportList = oparen *> (ExportList <$> pBracketedInner) <* cparen <|> pure NoExportList

pUse :: Parser (Import BufferSpan)
pUse = Import <$> require (ITvarid "use") <*> pure Use <*> pImportName <*> optional pImportAs <*> pImportList <* many (require ITsemi)

pImport :: Parser (Import BufferSpan)
pImport = import_ <*> (qualified_name <|> name_qualified) <*> optional pImportAs <*> pImportList <* many (require ITsemi)
  where
    import_ = fmap uncurry Import <$> require ITimport
    qualified_name = (,) <$> pQualified <*> pImportName
    name_qualified =  flip (,) <$> pImportName <*> pQualifiedPost

pQualified :: Parser ImportQualification
pQualified = require ITqualified *> pure Qualified

pQualifiedPost :: Parser ImportQualification
pQualifiedPost = require ITqualified *> pure QualifiedPost <|> pure Unqualified

pImportName :: Parser (ImportName BufferSpan)
pImportName = ImportName <$> pImportPackage <*> pModuleName

pImportPackage :: Parser PackageName
pImportPackage = (token $ \ case
  L _ (ITstring _ name) -> Just $ PackageName name
  _ -> Nothing) <|> pure NoPackageName

pImportAs :: Parser (ModuleName BufferSpan)
pImportAs = require ITas *> pModuleName

pImportList :: Parser (ImportList BufferSpan)
pImportList = importList <*> items <|> pure NoImportList
  where
    importList = require IThiding *> pure HidingList <|> pure ImportList
    items = oparen *> pBracketedInner <* cparen

pModuleBody :: Parser [Node BufferSpan]
pModuleBody = many pNode <* eof

pNode :: Parser (Node BufferSpan)
pNode = pMethodDefinition <|> pMethodChain <|> pPragma <|> pAnyToken

pMethodDefinition :: Parser (Node BufferSpan)
pMethodDefinition = MethodDefinition <$> pMethod

data WithStackTrace = WithStackTrace | WithoutStackTrace
  deriving (Eq, Show)

data Method loc = Method {
  dot :: loc
, name :: MethodName loc
, withStackTrace :: WithStackTrace
, context :: [Type loc]
, arguments :: [Type loc]
, subject :: Type loc
, result :: Type loc
, definitionDot :: loc
, definitionName :: loc
} deriving (Eq, Show, Functor)

pMethod :: Parser (Method BufferSpan)
pMethod = do
  dot <- pMethodDot
  name <- pMethodName
  ((withStackTrace, context), t) <- first splitWithStackTrace . splitContext <$> pTypeSignature
  case reverse $ functionTypeAsList t of
    result : subject : (reverse -> arguments) -> do
      _ <- optional (require ITsemi)
      definitionDot <- pMethodDot
      definitionName <- pDefinitionName name
      return Method{..}
    _ -> error InvalidMethodArity
  where
    pMethodDot :: Parser BufferSpan
    pMethodDot = atStartOfLine PrefixProjection

    pTypeSignature :: Parser (Type BufferSpan)
    pTypeSignature = require (ITdcolon NormalSyntax) >> pType

    pDefinitionName :: MethodName BufferSpan -> Parser BufferSpan
    pDefinitionName (MethodName _ expected) = do
      st <- getParserState
      case st.stateInput.tokens of
        L loc (ITvarid name) : _
          | name == expected -> anySingle >> return loc
          | otherwise -> error $ ExpectingMethodName expected (Just name)
        _ -> error $ ExpectingMethodName expected Nothing

    splitContext :: Type loc -> ([Type loc], Type loc)
    splitContext = \ case
      TypeContext c t -> case splitContext t of
        (cs, ts) -> (c : cs, ts)
      t -> ([], t)

    splitWithStackTrace :: [Type loc] -> (WithStackTrace, [Type loc])
    splitWithStackTrace = go
      where
        go :: [Type loc] -> (WithStackTrace, [Type loc])
        go = \ case
          [] ->
            (WithoutStackTrace, [])
          TypeName _ Nothing "WithStackTrace" : xs ->
            (WithStackTrace, xs)
          x : xs ->
            (x :) <$> go xs

    functionTypeAsList :: Type loc -> [Type loc]
    functionTypeAsList = \ case
      FunctionType t1 t2 -> t1 : functionTypeAsList t2
      t -> [t]

atStartOfLine :: Lexer.Token -> Parser BufferSpan
atStartOfLine expected = do
  st <- getParserState
  case st.stateInput.tokens of
    L loc t : _ | t == expected && loc.startColumn /= 1 -> indentationError pos1 loc
    _ -> require expected

data MethodName loc = MethodName {
  loc :: loc
, name :: FastString
} deriving (Eq, Show, Functor)

pMethodName :: Parser (MethodName BufferSpan)
pMethodName = uncurry MethodName <$> varid

data Type loc =
    TypeVariable FastString
  | TypeName loc (Maybe FastString) FastString
  | TypeLiteral String
  | Tuple [Type loc]
  | ListOf (Type loc)
  | TypeApplication (Type loc) (Type loc)
  | FunctionType (Type loc) (Type loc)
  | TypeContext (Type loc) (Type loc)
  deriving (Eq, Show, Functor)

pType :: Parser (Type BufferSpan)
pType = makeExprParser term [
    [InfixL $ pure TypeApplication]
  , [InfixR $ FunctionType <$ require (ITrarrow NormalSyntax)]
  , [InfixR $ TypeContext <$ require (ITdarrow NormalSyntax)]
  ]
  where
    term :: Parser (Type BufferSpan)
    term = variable <|> name <|> tuple <|> list

    variable :: Parser (Type BufferSpan)
    variable = do
      (_, a) <- varid
      return (TypeVariable a)

    name :: Parser (Type BufferSpan)
    name = token \ case
      L loc (ITconid n) -> Just $ TypeName loc Nothing n
      L loc (ITqconid (qualified, n)) -> Just $ TypeName loc (Just qualified) n
      _ -> Nothing

    tuple :: Parser (Type BufferSpan)
    tuple = oparen >> Tuple <$> pType `sepBy` comma <* cparen

    list :: Parser (Type BufferSpan)
    list = obrack >> ListOf <$> pType <* cbrack

pMethodChain :: Parser (Node BufferSpan)
pMethodChain = MethodChain <$> pSubject <*> many pMethodCall

pMethodCall :: Parser (MethodCall BufferSpan)
pMethodCall = require InfixProjection *> do
  (loc, name) <- varid
  MethodCall loc name <$> pArguments loc

pArguments :: BufferSpan -> Parser (Arguments BufferSpan)
pArguments loc = arguments <|> pure NoArguments
  where
    arguments :: Parser (Arguments BufferSpan)
    arguments = do
      start <- token $ \ case
        L start IToparen | loc.end == start.start -> Just start
        _ -> Nothing
      args <- (:) <$> pArgument start <*> many (comma >>= pArgument)
      end <- cparen
      return $ Arguments (start.merge end) args

pArgument :: BufferSpan -> Parser (Argument BufferSpan)
pArgument start = Argument start <$> some pNode

pSubject :: Parser (Subject BufferSpan)
pSubject = pLiteralString <|> pInterpolatedString <|> pBracketed <|> pName <|> pQualifiedName

pLiteralString :: Parser (Subject BufferSpan)
pLiteralString = token \ case
  L loc (ITstring (SourceText src) _) -> Just $ LiteralString (Literal loc src)
  _ -> Nothing

pInterpolatedString :: Parser (Subject BufferSpan)
pInterpolatedString = pTokenBegin <*> pExpression
  where
    pExpression :: Parser (Expression BufferSpan)
    pExpression = Expression <$> many pNode <*> (pEnd <|> error UnterminatedStringInterpolation)

    pEnd :: Parser (End BufferSpan)
    pEnd = pTokenEnd <|> pTokenEndBegin <*> pExpression

pBracketed :: Parser (Subject BufferSpan)
pBracketed = join $ token \ (L loc t) -> case tokenClass t of
  OpenBracket close -> Just $ bracketed loc <$> pBracketedInner <*> require close
  _ -> Nothing
  where
    bracketed :: BufferSpan -> [[Node BufferSpan]] -> BufferSpan -> Subject BufferSpan
    bracketed start nodes end = Bracketed (start.merge end) nodes

pBracketedInner :: Parser [[Node BufferSpan]]
pBracketedInner = many pNode `sepBy` comma

pName :: Parser (Subject BufferSpan)
pName = do
  (loc, name) <- varid
  Name loc name <$> pArguments loc

pQualifiedName :: Parser (Subject BufferSpan)
pQualifiedName = do
  (loc, (module_, name)) <- qvarid
  QualifiedName loc module_ name <$> pArguments loc

varid :: Parser (BufferSpan, FastString)
varid = token \ case
  L loc (ITvarid name) -> Just (loc, name)
  _ -> Nothing

qvarid :: Parser (BufferSpan, (FastString, FastString))
qvarid = token \ case
  L loc (ITqvarid name) -> Just (loc, name)
  _ -> Nothing

pPragma :: Parser (Node BufferSpan)
pPragma = Pragma <$> open <*> nodes <* close
  where
    open :: Parser BufferSpan
    open = token \ (L loc t) -> case tokenClass t of
      OpenPragma -> Just loc
      _ -> Nothing

    nodes :: Parser [Node BufferSpan]
    nodes = map tokenAsNode <$> many exceptClose

    exceptClose :: Parser Token
    exceptClose = token \ case
      L _ ITclose_prag -> Nothing
      t -> Just t

    close :: Parser ()
    close = void (require ITclose_prag) <|> eof

tokenAsNode :: Token -> Node BufferSpan
tokenAsNode (L loc t) = Token loc t

pAnyToken :: Parser (Node BufferSpan)
pAnyToken = token \ case
  TokenEndBegin {} -> Nothing
  TokenEnd {} -> Nothing
  L _ t | (tokenMetadata t).excludedByAnyToken -> Nothing
  t -> Just $ tokenAsNode t

data TokenClass =
    Other
  | OpenPragma
  | OpenBracket Lexer.Token
  deriving (Eq, Show)

data TokenMetadata = TokenMetadata {
  class_ :: TokenClass
, excludedByAnyToken :: Bool
, name :: Maybe String
}

instance IsString TokenMetadata where
  fromString = \ case
    "" -> TokenMetadata Other False Nothing
    name -> TokenMetadata Other False (Just name)

tokenMetadata :: Lexer.Token -> TokenMetadata
tokenMetadata = \ case
  ITcomma -> excluded ","

  IToparen -> (excluded "(") { class_ = OpenBracket ITcparen}
  ITcparen -> excluded ")"

  ITobrack -> (excluded "[") { class_ = OpenBracket ITcbrack}
  ITcbrack -> excluded "]"

  ITocurly -> (excluded "{") { class_ = OpenBracket ITccurly}
  ITccurly -> excluded "}"

  IToubxparen -> (excluded "(#") { class_ = OpenBracket ITcubxparen }
  ITcubxparen -> excluded "#)"

  ITopabrack -> (excluded "[:") { class_ = OpenBracket ITcpabrack }
  ITcpabrack -> excluded ":]"

  ITopenTypQuote -> excluded "[t|"
  ITopenDecQuote -> excluded "[d|"
  ITopenPatQuote -> excluded "[p|"
  ITopenExpQuote NoE NormalSyntax -> excluded "[|"
  ITopenExpQuote HasE NormalSyntax -> excluded "[e|"
  ITopenExpQuote NoE UnicodeSyntax -> excluded "⟦"
  ITcloseQuote NormalSyntax -> excluded "|]"
  ITcloseQuote UnicodeSyntax -> excluded "⟧"
  ITopenTExpQuote NoE -> excluded "[||"
  ITopenTExpQuote HasE -> excluded "[e||"
  ITcloseTExpQuote -> excluded "||]"

  IToparenbar NormalSyntax -> excluded "(|"
  IToparenbar UnicodeSyntax -> excluded "⦇"
  ITcparenbar NormalSyntax -> excluded "|)"
  ITcparenbar UnicodeSyntax -> excluded "⦈"
  ITequal -> "="
  ITdcolon NormalSyntax -> "::"
  ITdcolon UnicodeSyntax -> "∷"

  ITinline_prag {} -> openPragma
  ITopaque_prag {} -> openPragma
  ITspec_prag {} -> openPragma
  ITspec_inline_prag {} -> openPragma
  ITsource_prag {} -> openPragma
  ITrules_prag {} -> openPragma
  ITwarning_prag {} -> openPragma
  ITdeprecated_prag {} -> openPragma
  ITline_prag {} -> openPragma
  ITcolumn_prag {} -> openPragma
  ITscc_prag {} -> openPragma
  ITunpack_prag {} -> openPragma
  ITnounpack_prag {} -> openPragma
  ITann_prag {} -> openPragma
  ITcomplete_prag {} -> openPragma
  IToptions_prag {} -> openPragma
  ITinclude_prag {} -> openPragma
  ITlanguage_prag {} -> openPragma
  ITminimal_prag {} -> openPragma
  IToverlappable_prag {} -> openPragma
  IToverlapping_prag {} -> openPragma
  IToverlaps_prag {} -> openPragma
  ITincoherent_prag {} -> openPragma
  ITctype {} -> openPragma
  ITcomment_line_prag {} -> openPragma

  _ -> TokenMetadata Other False Nothing
  where
    excluded :: String -> TokenMetadata
    excluded = TokenMetadata Other True . Just

    openPragma :: TokenMetadata
    openPragma = TokenMetadata OpenPragma False Nothing

tokenClass :: Lexer.Token -> TokenClass
tokenClass t = (tokenMetadata t).class_

tokenName :: Lexer.Token -> Maybe String
tokenName t = (tokenMetadata t).name

comma :: Parser BufferSpan
comma = require ITcomma

oparen :: Parser BufferSpan
oparen = require IToparen

cparen :: Parser BufferSpan
cparen = require ITcparen

obrack :: Parser BufferSpan
obrack = require ITobrack

cbrack :: Parser BufferSpan
cbrack = require ITcbrack

pTokenBegin :: Parser (Expression BufferSpan -> Subject BufferSpan)
pTokenBegin = token \ case
  TokenBegin loc src -> Just $ LiteralString . Begin loc src
  _ -> Nothing

pTokenEnd :: Parser (End BufferSpan)
pTokenEnd = token \ case
  TokenEnd loc src -> Just (End loc src)
  _ -> Nothing

pTokenEndBegin :: Parser (Expression BufferSpan -> End BufferSpan)
pTokenEndBegin = token \ case
  TokenEndBegin loc src -> Just (EndBegin loc src)
  _ -> Nothing

pattern TokenBegin :: BufferSpan -> FastString -> Token
pattern TokenBegin loc src <- L loc (ITstring_interpolation_begin (SourceText src) _)

pattern TokenEnd :: BufferSpan -> FastString -> Token
pattern TokenEnd loc src <- L loc (ITstring_interpolation_end (SourceText src) _)

pattern TokenEndBegin :: BufferSpan -> FastString -> Token
pattern TokenEndBegin loc src <- L loc (ITstring_interpolation_end_begin (SourceText src) _)

data Module loc = Module (ModuleHeader loc) [Import loc] [Node loc]
  deriving (Eq, Show, Functor)

data ModuleHeader loc = NoModuleHeader | ModuleHeader loc (ModuleName loc) (ExportList loc)
  deriving (Eq, Show, Functor)

data ModuleName loc = ModuleName loc (Maybe FastString) FastString
  deriving (Eq, Show, Functor)

data ExportList loc = NoExportList | ExportList [[Node loc]]
  deriving (Eq, Show, Functor)

data Import loc = Import {
  start :: loc
, qualification :: ImportQualification
, name :: ImportName loc
, as_name :: Maybe (ModuleName loc)
, import_list :: ImportList loc
} deriving (Eq, Show, Functor)

data ImportQualification = Use | Qualified | QualifiedPost | Unqualified
  deriving (Eq, Show)

data ImportName loc = ImportName {
  package :: PackageName
, name :: ModuleName loc
} deriving (Eq, Show, Functor)

data PackageName = NoPackageName | PackageName FastString
  deriving (Eq, Show)

data ImportList loc = NoImportList | ImportList [[Node loc]] | HidingList [[Node loc]]
  deriving (Eq, Show, Functor)

data Node loc =
    Token loc Lexer.Token
  | Pragma loc [Node loc]
  | MethodDefinition (Method loc)
  | MethodChain (Subject loc) [MethodCall loc]
  deriving (Eq, Show, Functor)

data Subject loc =
    LiteralString (LiteralString loc)
  | Bracketed loc [[Node loc]]
  | Name loc FastString (Arguments loc)
  | QualifiedName loc FastString FastString (Arguments loc)
  deriving (Eq, Show, Functor)

data Arguments loc = NoArguments | Arguments loc [Argument loc]
  deriving (Eq, Show, Functor)

data Argument loc = Argument loc (NonEmpty (Node loc))
  deriving (Eq, Show, Functor)

data MethodCall loc = MethodCall loc FastString (Arguments loc)
  deriving (Eq, Show, Functor)

data LiteralString loc = Literal loc FastString | Begin loc FastString (Expression loc)
  deriving (Eq, Show, Functor)

data Expression loc = Expression [Node loc] (End loc)
  deriving (Eq, Show, Functor)

data End loc = End loc FastString | EndBegin loc FastString (Expression loc)
  deriving (Eq, Show, Functor)

instance HasField "loc" (End loc) loc where
  getField = \ case
    End loc _ -> loc
    EndBegin loc _ _ -> loc

instance HasField "start" (Node BufferSpan) SrcLoc where
  getField = \ case
    Token loc _ -> loc.startLoc
    Pragma loc _ -> loc.startLoc
    MethodDefinition method -> method.dot.startLoc
    MethodChain subject _ -> subject.start

instance HasField "start" (Subject BufferSpan) SrcLoc where
  getField = \ case
    LiteralString (Literal loc _) -> loc.startLoc
    LiteralString (Begin loc _ _) -> loc.startLoc
    Bracketed loc _ -> loc.startLoc
    Name loc _ _ -> loc.startLoc
    QualifiedName loc _ _ _ -> loc.startLoc
