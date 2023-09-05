#!/usr/bin/env -S solid -package=doctest -package=markdown-unlit
import Text.Markdown.Unlit (CodeBlock(..))
import Text.Markdown.Unlit qualified as Unlit

import Test.DocTest.Internal.Run
import Test.DocTest.Internal.Location
import Test.DocTest.Internal.Parse (Module(..), DocTest(..))
import Test.DocTest.Internal.Parse qualified as DocTest

main :: IO ()
main = extract "README.md" >>= (runTests -< Process.args) >>= evaluateResult

extract :: FilePath -> IO [Module [Located DocTest]]
extract file = readFile file <&> (unlit file >>> toModule >>> List.singleton >>> parseModules)

unlit :: FilePath -> String -> [Located String]
unlit file = map toLocatedString . filter doctest? . Unlit.parse . unpack
  where
    toLocatedString :: CodeBlock -> Located String
    toLocatedString code = Located location string
      where
        location = Location file.toString.unpack code.codeBlockStartLine
        string = (code.codeBlockContent.map pack).unlines

    doctest? :: Unlit.CodeBlock -> Bool
    doctest? c = "repl" `elem` Unlit.codeBlockClasses c

toModule :: [Located String] -> Module (Located String)
toModule code = Module {
  moduleName = "Main"
, moduleSetup = Nothing
, moduleContent = code
}

parseModules :: [Module (Located String)] -> [Module [Located DocTest]]
parseModules = DocTest.parseModules . map (fmap (fmap unpack))

runTests :: [String] -> [Module [Located DocTest]] -> IO Result
runTests args = runDocTests defaultConfig { ghcOptions = args.map unpack, repl = ("solid", ["repl"]) }
