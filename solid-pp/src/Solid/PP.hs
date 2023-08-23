{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
module Solid.PP (
  main

, Result(..)
, run

, Language
, language

, LanguageFlag(..)
, Extension(..)
, showExtension
, extensions

#ifdef TEST
, Module
, usedModules
, ModuleHeader(..)
, parseModuleHeader
#endif
) where

import           Prelude ()
import           Solid.PP.IO hiding (concatMap)

import           Data.Char
import           Data.Word
import qualified Data.Text as T
import qualified Data.ByteString.Short as SB
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified GHC.Data.FastString as FastString
import           Data.Coerce (coerce)

import           Solid.PP.DList
import           Solid.PP.Edit (Edit(..), edit)
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

wellKnownModules :: Set Module
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

toStringModule :: Module
toStringModule = "Solid.ToString"

newtype Module = Module FastString
  deriving newtype (Eq, Show, IsString)

instance Ord Module where
  compare = coerce FastString.uniqCompareFS

data Result = Failure String | Success
  deriving (Eq, Show)

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
preProcesses src dst input = case parse extensions src input of
  Left err -> return (Failure err)
  Right nodes -> withFile dst WriteMode $ \ h -> do
    edit h input $ maybe id (:) (addImplicitImports nodes) (pp nodes)
    return Success

addImplicitImports :: [Node] -> Maybe Edit
addImplicitImports nodes = case parseModuleHeader nodes of
  Empty -> Nothing
  ModuleHeader self offset file line -> do
    let
      importsWithoutSelf :: Set Module
      importsWithoutSelf = maybe id Set.delete self imports
    case Set.null importsWithoutSelf of
      True -> Nothing
      False -> Just $ insert offset $ "\n" <> formatImports importsWithoutSelf <> linePragma line file
  NoModuleHeader offset file line -> do
    case Set.null imports of
      True -> Nothing
      False -> Just $ insert offset $ formatImports imports <> linePragma line file

  where
    imports :: Set Module
    imports = Set.intersection (usedModules nodes) wellKnownModules

    formatImports :: Set Module -> Text
    formatImports = T.unlines . map formatImport . Set.toList

    formatImport :: Module -> Text
    formatImport (Module m) = "import qualified " <> m.toText

    insert :: Int -> Text -> Edit
    insert offset = Replace Nothing offset 0

usedModules :: [NodeWith loc] -> Set Module
usedModules = Set.fromList . (.build) . fromNodes . map void
  where
    fromNodes :: [NodeWith ()] -> DList Module
    fromNodes = concatMap fromNode

    fromNode :: NodeWith () -> DList Module
    fromNode = \ case
      Token () (ITqvarid (m, _)) -> singleton (Module m)
      Token () (_ :: Token) -> mempty
      LiteralString (Begin () (_ :: String) expression) -> singleton toStringModule <> fromExpression expression
      LiteralString (Literal () (_ :: String)) -> mempty

    fromExpression :: ExpressionWith () -> DList Module
    fromExpression (Expression nodes end) = fromNodes nodes <> case end of
      EndBegin () (_ :: String) expression -> fromExpression expression
      End () (_ :: String) -> mempty

data ModuleHeader = Empty | ModuleHeader (Maybe Module) Int FilePath Int | NoModuleHeader Int FilePath Int
  deriving (Eq, Show)

parseModuleHeader :: [Node] -> ModuleHeader
parseModuleHeader nodes = afterExportList
  where
    afterExportList :: ModuleHeader
    afterExportList = case seekTo ITmodule nodes of
      Just (_, rest) -> case seekTo ITwhere rest of
        Just (loc, _) -> do
          let
            self :: Maybe Module
            self = case rest of
              Token _ (ITconid name) : _ -> Just (Module name)
              Token _ (ITqconid (qualified, name)) : _ -> Just (Module $ qualified <> "." <> name)
              _ -> Nothing
          ModuleHeader self loc.end loc.file loc.endLine
        Nothing -> Empty
      Nothing -> case nodes of
        Token loc _ : _ -> NoModuleHeader loc.start loc.file loc.startLine
        LiteralString (Literal loc _) : _ -> NoModuleHeader loc.start loc.file loc.startLine
        LiteralString (Begin loc _ _) : _ -> NoModuleHeader loc.start loc.file loc.startLine
        [] -> Empty

seekTo :: Token -> [Node] -> Maybe (BufferSpan, [Node])
seekTo t nodes = case dropWhile (not . isToken t) nodes of
  Token loc _ : rest -> Just (loc, rest)
  _ -> Nothing

isToken :: Token -> Node -> Bool
isToken expected = \ case
  Token _ actual -> actual == expected
  LiteralString {} -> False

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

replaceBufferSpan :: BufferSpan -> String -> Edit
replaceBufferSpan loc = Replace (Just loc.startColumn) loc.start loc.length . pack

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
  | otherwise = singleton $ replaceBufferSpan loc new
  where
    new = unescapeString old

pp :: [Node] -> [Edit]
pp = (.build) . ppNodes
  where
    ppNodes :: [Node] -> DList Edit
    ppNodes = concatMap ppNode

    ppNode :: Node -> DList Edit
    ppNode = \ case
      Token loc t -> ppToken loc t
      LiteralString string -> ppLiteralString string

    ppToken :: BufferSpan -> Token -> DList Edit
    ppToken loc = \ case
      ITvarid identifier -> desugarIdentifier loc.start loc.end identifier
      ITqvarid (module_, name) -> desugarQualifiedName loc module_ name
      _ -> mempty

    ppLiteralString :: LiteralString BufferSpan -> DList Edit
    ppLiteralString = \ case
      Literal loc src -> unescapeStringLiteral loc src
      Begin loc src expression -> replace loc (lambdaAbstract expression <> beginInterpolation src) <> ppExpression 1 expression

    ppExpression :: Int -> Expression -> DList Edit
    ppExpression n = \ case
      Expression [] end -> insert end.loc.startLoc (abstractionParam n "") <> ppEnd (succ n) end
      Expression nodes end -> ppNodes nodes <> ppEnd n end

    ppEnd :: Int -> End BufferSpan -> DList Edit
    ppEnd n = \ case
      End loc src -> replace loc (endInterpolation src)
      EndBegin loc src expression -> replace loc (endBeginInterpolation src) <> ppExpression n expression

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

    endInterpolation :: String -> DString
    endInterpolation src = case unescape src of
      Nothing -> "))"
      Just string -> ") <> \"" <> string <> "\")"

    endBeginInterpolation :: String -> DString
    endBeginInterpolation src = ") <> " <> beginInterpolation src

    replace :: BufferSpan -> DString -> DList Edit
    replace loc = singleton . replaceBufferSpan loc . (.build)

    insert :: SrcLoc -> String -> DList Edit
    insert loc = singleton . Replace (Just $ StartColumn loc.column) loc.offset 0 . pack

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
