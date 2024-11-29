{-# LANGUAGE OverloadedStrings #-}
module HieSpec (spec) where

import Prelude ()
import Solid.PP.IO hiding (span, (<>))

import Test.Hspec
import System.IO.Temp
import System.Process
import System.FilePath

import Data.Map qualified as Map
import Data.Text qualified as Text

import GHC.Fingerprint
import GHC.Version qualified as GHC

import GHC.Types.SrcLoc
import GHC.Types.Name.Cache
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Utils.Outputable

import Solid.PP qualified as PP

force :: Bool
force = True

tryReadFile :: FilePath -> IO (Either IOError Text)
tryReadFile = try . readFile

ensureFile :: FilePath -> Text -> IO ()
ensureFile name new = do
  old <- tryReadFile name
  when (old /= Right new) $ do
    writeFile name new

ensurePp :: FilePath -> FilePath -> FilePath -> IO ()
ensurePp src cur dst = withSystemTempDirectory "solid-pp" $ \ dir -> do
  let tmp = dir </> takeFileName dst
  PP.run src cur tmp `shouldReturn` PP.Success
  readFile tmp >>= ensureFile dst

mkHash :: FilePath -> IO Text
mkHash src = do
  hash <- getFileHash src
  return . pack . show $ fingerprintFingerprints [hash, fingerprintString GHC.cProjectGitCommitId]

mkHieFile :: FilePath -> FilePath -> IO (HieASTs Int)
mkHieFile src name = withSystemTempDirectory "solid" $ \ dir -> do
  callProcess "ghc" ["-v0", "-fno-code", "-hiedir", dir,  "-fwrite-ide-info", src]
  nameCache <- initNameCache 'z' []
  hie_asts . hie_file_result <$> readHieFile nameCache (dir </> name)

runTest :: FilePath -> IO ()
runTest name = do
  let
    dir = "test" </> "hie-dump" </> name
    src = dir </> "Foo.hs"
    dst = dir </> "Foo.pp.hs"
    hashFile = dir </> "hash"
  ensurePp "Foo.hs" src dst
  newHash <- mkHash dst
  oldHash <- tryReadFile hashFile
  when (force || oldHash /= Right newHash) $ do
    ensureFile hashFile newHash
    hieFile <- mkHieFile dst "Foo.hie"
    source <- lines <$> readFile src
    ensureFile (dir </> "dump") (pprHieFile source hieFile)

stripTrailingSpaces :: Text -> Text
stripTrailingSpaces = Text.unlines . map Text.stripEnd . Text.lines

pprHieFile :: [Text] -> HieASTs TypeIndex -> Text
pprHieFile source = stripTrailingSpaces . pack . showSDocUnsafe . vcat . map pprAst . Map.toList . getAsts
  where
    pprAst :: (HiePath, HieAST TypeIndex) -> SDoc
    pprAst (file, node) = vcat [
        "File: " <> ppr file
      , pprNode node
      ]

    pprNode :: HieAST TypeIndex -> SDoc
    pprNode (Node info span nodes) = vcat [
        sourceSnippet span source
      , ppr span <> ":" <+> ppr info
      , nest 2 (vcat $ map pprNode nodes)
      ]

sourceSnippet :: Span -> [Text] -> SDoc
sourceSnippet loc =
    vcat . map fromString
  . decorate
  . map unpack
  . drop (pred startLine) . take endLine
  where
    decorate :: [String] -> [String]
    decorate = \ case
      [] -> []
      [l] -> [
          "  |"
        , addLineNumber startLine l
        , "  | " ++ replicate (pred startColumn) ' ' ++ replicate (max 1 $ endColumn - startColumn) '^'
        ]
      l : ls ->
        ("  | " ++ replicate (pred startColumn) ' ' ++ replicate (length l - startColumn + 1) '_') :
        (addLineNumbers startLine $ l : ls) ++
        ["  | " ++ replicate (pred endColumn) '^']

    addLineNumbers :: Int -> [String] -> [String]
    addLineNumbers n = zipWith addLineNumber [n ..]

    addLineNumber :: Int -> String -> String
    addLineNumber n l = show n ++ " | " ++ l

    startLine :: Int
    startLine = srcSpanStartLine loc

    endLine :: Int
    endLine = srcSpanEndLine loc

    startColumn :: Int
    startColumn = srcSpanStartCol loc

    endColumn :: Int
    endColumn = srcSpanEndCol loc

test :: String -> Spec
test = it <*> runTest

ftest :: String -> Spec
ftest = focus . test

spec :: Spec
spec = do
  test "use"
  test "use-as"
  test "use-qualified-name"
  test "use-qualified-name-as"
  test "import-qualified-as"
  where
    _ignore = ftest
