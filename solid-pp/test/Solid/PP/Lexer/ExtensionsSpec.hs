{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.PP.Lexer.ExtensionsSpec (spec) where

import           Prelude ()
import           Solid.PP.IO

import           Test.Hspec

import           Data.List ((\\))
import qualified Data.List as L
import           GHC.IsList
import qualified GHC.Data.EnumSet as EnumSet

import           Solid.PP.Lexer (stringToStringBuffer)
import           Solid.PP.Lexer.Extensions hiding (extensionsFromModuleHeader)
import qualified Solid.PP.Lexer.Extensions as Lexer

instance IsString StringBuffer where
  fromString = stringToStringBuffer

instance IsList StringBuffer where
  type Item StringBuffer = String
  fromList = fromString . L.unlines
  toList = undefined

instance (Enum a, Show a) => Show (EnumSet a) where
  showsPrec n = showsPrec n . toList

instance (Enum a, Eq a) => Eq (EnumSet a) where
  a == b = toList a == toList b

instance Enum a => IsList (EnumSet a) where
  type Item (EnumSet a) = a
  fromList = EnumSet.fromList
  toList = EnumSet.toList

extensionsFromModuleHeader :: Maybe Language -> [ExtensionFlag] -> StringBuffer -> EnumSet Extension
extensionsFromModuleHeader language extensions = Lexer.extensionsFromModuleHeader language extensions "main.hs"

haskell98 :: EnumSet Extension
haskell98 = extensionsFromModuleHeader (Just Haskell98) [] ""

ghc2021 :: EnumSet Extension
ghc2021 = extensionsFromModuleHeader (Just GHC2021) [] ""

ghc2024 :: EnumSet Extension
ghc2024 = extensionsFromModuleHeader (Just GHC2024) [] ""

spec :: Spec
spec = do
  describe "parseLanguageFlag" do
    forM_ @[] [minBound .. maxBound] $ \ language -> do
      it ("parses " <> show language) do
        parseLanguageFlag (showLanguageFlag language) `shouldBe` Just language

  describe "parseExtensionFlag" do
    forM_ @[] [minBound .. maxBound] $ \ extension -> do
      it ("parses " <> show extension) do
        parseExtensionFlag (showExtensionFlag (On extension)) `shouldBe` Just (On extension)
        parseExtensionFlag (showExtensionFlag (Off extension)) `shouldBe` Just (Off extension)

  describe "knownLanguagePragmas" $ do
    it "includes all language extensions" $ do
      let
        names = map extensionName [minBound .. maxBound]
        extensions = names ++ map (mappend "No" ) names
      extensions \\ knownLanguagePragmas `shouldBe` []

  describe "extensionsFromModuleHeader" do
    it "applies default extensions" do
      extensionsFromModuleHeader Nothing [On LambdaCase] "" `shouldBe` [LambdaCase]

    it "applies extensions from pragmas" do
      extensionsFromModuleHeader Nothing [] "{-# LANGUAGE LambdaCase #-}" `shouldBe` [LambdaCase]

    it "gives extensions from pragmas precedence over default extensions" do
      extensionsFromModuleHeader Nothing [On LambdaCase] "{-# LANGUAGE NoLambdaCase #-}" `shouldBe` []

    it "gives later extensions precedence" do
      extensionsFromModuleHeader Nothing [On LambdaCase] [
          "{-# LANGUAGE LambdaCase #-}"
        , "{-# LANGUAGE NoLambdaCase #-}"
        , "{-# LANGUAGE NoOverloadedStrings #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        ] `shouldBe` [OverloadedStrings]

    it "applies implied extensions" do
      extensionsFromModuleHeader Nothing [On TypeFamilies] "" `shouldBe` [TypeFamilies, MonoLocalBinds, KindSignatures, ExplicitNamespaces]

    it "does not negate implied extensions" do
      extensionsFromModuleHeader Nothing [On TypeFamilies] "{-# LANGUAGE NoTypeFamilies #-}" `shouldBe` [MonoLocalBinds, KindSignatures, ExplicitNamespaces]

    it "applies default language" do
      extensionsFromModuleHeader (Just Haskell98) [] "" `shouldBe` haskell98
      haskell98 `shouldBe` [
          MonomorphismRestriction
        , DeepSubsumption
        , ImplicitPrelude
        , NPlusKPatterns
        , DatatypeContexts
        , NondecreasingIndentation
        , TraditionalRecordSyntax
        , StarIsType
        , CUSKs
        , FieldSelectors
        , ListTuplePuns
        ]

    it "applies language from pragmas" do
      extensionsFromModuleHeader Nothing [] "{-# LANGUAGE GHC2024 #-}" `shouldBe` ghc2024

    it "gives language from pragmas precedence" do
      extensionsFromModuleHeader (Just GHC2021) [] "{-# LANGUAGE GHC2024 #-}" `shouldBe` ghc2024

    it "gives later language pragmas precedence" do
      extensionsFromModuleHeader Nothing [] [
          "{-# LANGUAGE GHC2024 #-}"
        , "{-# LANGUAGE GHC2021 #-}"
        ] `shouldBe` ghc2021

    it "gives explicit extensions precedence over language implied extensions" do
      EnumSet.member LambdaCase ghc2024 `shouldBe` True
      extensionsFromModuleHeader Nothing [] [
          "{-# LANGUAGE NoLambdaCase #-}"
        , "{-# LANGUAGE GHC2024 #-}"
        ] `shouldBe` EnumSet.delete LambdaCase ghc2024
