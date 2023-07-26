{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
module Solid.PP (
  main

, Result(..)
, run
, Extension
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

extensions :: [Extension]
extensions = [
    DataKinds
  , DeriveAnyClass
  , DuplicateRecordFields
  , LambdaCase
  , OverloadedRecordDot
  , OverloadedStrings
  ]

wellKnownModules :: Set Module
wellKnownModules = Set.fromList [
    "ByteString"
  , "Directory"
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

usedModules :: [Node] -> Set Module
usedModules = flip modules Set.empty
  where
    modules :: [Node] -> Set Module -> Set Module
    modules nodes ms = foldl' (flip fromNodes) ms nodes

    fromNodes :: Node -> Set Module -> Set Module
    fromNodes = \ case
      Token _ (ITqvarid (m, _)) -> Set.insert (Module m)
      Token _ _ -> id
      LiteralString (Begin _ _ expression) -> (Set.singleton toStringModule <>) . fromExpression expression
      LiteralString (Literal _ _) -> id

    fromExpression :: Expression -> Set Module -> Set Module
    fromExpression (Expression nodes end) = modules nodes . case end of
      EndBegin _ _ expression -> fromExpression expression
      End _ _ -> id

data ModuleHeader = Empty | ModuleHeader (Maybe Module) Int FilePath Int | NoModuleHeader Int FilePath Int
  deriving (Eq, Show)

parseModuleHeader :: [Node] -> ModuleHeader
parseModuleHeader nodes = afterExportList
  where
    afterExportList :: ModuleHeader
    afterExportList = case dropWhile (token (/= ITmodule)) nodes of
      Token _ ITmodule : rest -> case dropWhile (token (/= ITwhere)) rest of
        Token loc ITwhere : _ -> do
          let
            self :: Maybe Module
            self = case dropWhile (token isComment) rest of
              Token _ (ITconid name) : _ -> Just (Module name)
              Token _ (ITqconid (qualified, name)) : _ -> Just (Module $ qualified <> "." <> name)
              _ -> Nothing
          ModuleHeader self loc.end loc.file loc.endLine
        _ -> Empty
      _ -> afterLanguagePragmas

    afterLanguagePragmas :: ModuleHeader
    afterLanguagePragmas = case dropWhile (token isComment) nodes of
      Token loc _ : _ -> NoModuleHeader loc.start loc.file loc.startLine
      _ -> Empty

isComment :: Token -> Bool
isComment = \ case
  ITlineComment {} -> True
  ITblockComment {} -> True
  _ -> False

token :: (Token -> Bool) -> Node -> Bool
token p = \ case
  Token _ t -> p t
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
pp = (.build) . onNodes
  where
    onNodes :: [Node] -> DList Edit
    onNodes = concatMap onNode

    onNode :: Node -> DList Edit
    onNode = \ case
      Token loc t -> onToken loc t
      LiteralString string -> onLiteralString string

    onToken :: BufferSpan -> Token -> DList Edit
    onToken loc = \ case
      ITvarid identifier -> desugarIdentifier loc.start loc.end identifier
      ITqvarid (succ . lengthFS -> offset, identifier) -> desugarIdentifier (loc.start + offset) loc.end identifier
      _ -> mempty

    onLiteralString :: LiteralString BufferSpan -> DList Edit
    onLiteralString = \ case
      Literal loc src -> unescapeStringLiteral loc src
      Begin loc src expression -> replace loc (lambdaAbstract expression <> unescape src <> beginInterpolation) <> onExpression 1 expression

    onExpression :: Int -> Expression -> DList Edit
    onExpression n = \ case
      Expression [] end -> insert end.loc (abstractionParam n "") <> onEnd (succ n) end
      Expression nodes end -> onNodes nodes <> onEnd n end

    onEnd :: Int -> End BufferSpan -> DList Edit
    onEnd n = \ case
      End loc src -> replace loc (endInterpolation <> unescape src <> "\")")
      EndBegin loc src expression -> replace loc (endInterpolation <> unescape src <> beginInterpolation) <> onExpression n expression

    unescape :: String -> DString
    unescape = fromString . unescapeString . init . tail

    beginInterpolation :: DString
    beginInterpolation = "\" <> Solid.ToString.toString ("

    endInterpolation :: DString
    endInterpolation = ") <> \""

    replace :: BufferSpan -> DString -> DList Edit
    replace loc = singleton . replaceBufferSpan loc . (.build)

    insert :: BufferSpan -> String -> DList Edit
    insert loc = singleton . Replace (Just loc.startColumn) loc.start 0 . pack

lambdaAbstract :: Expression -> DString
lambdaAbstract = lambda . countAbstractions
  where
    lambda :: Int -> DString
    lambda n
      | n == 0 = "(\""
      | otherwise = "(\\" <> params <> " -> \""
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
