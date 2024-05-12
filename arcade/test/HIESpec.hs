{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module HIESpec (spec) where

import Prelude ()
import HaskellPrelude

import           Prelude hiding (writeFile)
import qualified Prelude

import           System.Process
import           Test.Mockery.Directory
import           Data.String.QQ
import           Test.Hspec

import qualified T
import           Data.Location
import           HIE hiding (runHie)
import qualified HIE
import           Scribe.Config

writeFile :: FilePath -> String -> IO ()
writeFile file contents = touch file >> Prelude.writeFile file contents

verbose :: Bool
verbose = False

dumpHie :: [String] -> [String]
dumpHie = if verbose then ("-ddump-hie" :) else id

obfuscateSourceLocations :: String -> String
obfuscateSourceLocations = T.unpack . T.replace ":" " : " . T.pack

runHie :: HieM a -> IO a
runHie = HIE.runHie defaultConfig

spec :: Spec
spec = around_ inTempDirectory $ do
  let source = "src/Foo.hs"
      location = Location source
      ghci = readProcess "ghci" (dumpHie ["-isrc", "-ignore-dot-ghci", "-v0", "-fwrite-ide-info", "-hiedir", hiedir, source]) "" >>= putStr . obfuscateSourceLocations
      create contents = writeFile source contents >> ghci

  describe "definition" $ do
    it "returns the definition site for global bindings" $ do
      create [s|
      module Foo where
      foo = 23
      bar = foo
      |]
      runHie (definition $ location 3 13) `shouldReturn` Just (location 2 7)

    it "returns the definition site for pattern bindings" $ do
      create [s|
      {-# LANGUAGE LambdaCase #-}
      module Foo where
      foo :: a -> a
      foo = \ case
        n -> n
      |]
      runHie (definition $ location 5 14) `shouldReturn` Just (location 5 9)

  describe "references" $ do
    it "returns all references for a name" $ do
      create [s|
      module Foo where
      foo = 23
      bar = foo
      |]
      runHie (references $ location 2 7) `shouldReturn` [
          (location 2 7, "reference")
        , (location 3 13, "reference")
        ]

    it "returns all references for a local name" $ do
      create [s|
      module Foo where
      foo = n
        where n = 23
      |]
      runHie (references $ location 3 15) `shouldReturn` [
          (location 2 13, "reference")
        , (location 3 15, "reference")
        ]

    it "returns all references for a name across modules" $ do
      writeFile "src/Bar.hs" [s|
      module Bar where
      bar = 23
      baz = bar
      |]
      create [s|
      module Foo where
      import Bar
      foo = bar
      |]
      runHie (references $ location 3 13) `shouldReturn` [
          (location 3 13, "reference")
        , (Location "src/Bar.hs" 2 7, "reference")
        , (Location "src/Bar.hs" 3 13, "reference")
        ]

  describe "rename" $ do
    it "renames all references of an identifier" $ do
      create [s|
      module Foo where
      foo = 23
      bar = foo + foo
      |]
      runHie $ rename "n" (location 2 7)
      readFile source `shouldReturn` [s|
      module Foo where
      n = 23
      bar = n + n
      |]

  describe "getType" $ do
    context "at a use site" $ do
      it "gets a list of possible types" $ do
        create [s|
        module Foo where
        foo = (++ "bar")
        |]
        runHie (getType $ location 2 16) `shouldReturn` [
            "[Char] -> [Char] -> [Char]"
          , "forall a. [a] -> [a] -> [a]"
          ]

    context "at a definition site" $ do
      it "gets a list of possible types" $ do
        create [s|
        module Foo where
        foo = (++ "bar")
        |]
        runHie (getType $ location 2 9) `shouldReturn` [
            "[Char] -> [Char]"
          ]

  describe "addTypeSignature" $ do
    it "adds a type signature" $ do
      create [s|
      module Foo where

      foo = (++ "bar")
      |]
      runHie $ addTypeSignature (location 3 7)
      readFile source `shouldReturn` [s|
      module Foo where

      foo :: [Char] -> [Char]
      foo = (++ "bar")
      |]

  describe "findHieFile" $ do
    it "finds .hie for source files" $ do
      let
        src = "src/Foo.hs"
        hie = "hie/Foo.hie"
      touch src
      touch hie
      runHie (findHieFile src) `shouldReturn` Just hie

    it "finds .hie for spec files" $ do
      let
        src = "test/FooSpec.hs"
        hie = "hie/FooSpec.hie"
      touch src
      touch hie
      runHie (findHieFile src) `shouldReturn` Just hie
