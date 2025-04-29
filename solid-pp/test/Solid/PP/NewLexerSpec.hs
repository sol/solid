{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Solid.PP.NewLexerSpec (spec) where

import           Prelude ()
import           Solid.PP.IO

import           Test.Hspec

import           Solid.PP (language, extensions)
import qualified Solid.PP.Lexer as Old
import           Solid.PP.Lexer hiding (toBufferSpan, tokenize)

import           Solid.PP.NewLexer (TokenType(..))
import qualified Solid.PP.NewLexer as New

ref :: HasCallStack => Text -> [WithBufferSpan Token]
ref = either error ((.tokens)) . Old.tokenize language extensions "" 1

tokenize :: Text -> [WithBufferSpan Token]
tokenize = foo . map toToken . New.tokenize

toBufferSpan :: New.SrcSpan -> BufferSpan
toBufferSpan loc = BufferSpan "" start.offset end.offset start.line end.line start.column end.column
  where
    start = loc.start
    end = loc.end

toToken :: New.Token -> WithBufferSpan Token
toToken token = L (toBufferSpan token.tokSpan) case token.tokType of
  TokKeyword text -> ITeof
  Identifier name -> ITvarid (toFastString name)
  QualifiedIdentifier mod name -> ITqvarid (toFastString mod, toFastString name)
  Constructor name -> ITconid (toFastString name)
  QualifiedConstructor mod name -> ITqconid (toFastString mod, toFastString name)
  IncompleteQualifiedName _ -> ITeof
  TokOperator "." -> ITproj True
  TokOperator text -> ITeof
  Integer text -> ITinteger (IL (sourceText text) False $ read (unpack text)) -- FIXME: read
  TokSymbol char -> ITeof
  TokComment text -> ITeof
  EndOfFile -> ITeof

toFastString :: Text -> FastString
toFastString = unpack >>> fromString -- FIXME

sourceText :: Text -> SourceText
sourceText = SourceText . toFastString

foo :: [WithBufferSpan Token] -> [WithBufferSpan Token]
foo = loop
  where
    loop = \ case
      [] -> []
      token : L loc (ITproj True) : tokens | (getLoc token).end == loc.start -> token : L loc (ITproj False) : loop tokens
      token : tokens -> token : loop tokens

test :: HasCallStack => Text -> Expectation
test input = do
  map unLoc actual `shouldBe` map unLoc expected
  tokenize input `shouldBe` ref input
  where
    actual = tokenize input
    expected = ref input

spec :: Spec
spec = do
{-
  fdescribe "foobar" $ do
    fit "accepts identifiers" $ do
      New.foobar "Foo.Bar.baz more input" `shouldBe` ("Foo.Bar", "baz", " more input")
      -}

  fdescribe "tokenize" $ do
    it "" $ do
      test "foo"

    it "" $ do
      test "_foo"

    it "" $ do
      test "Foo"

    it "" $ do
      test "Foo foo"

    it "" $ do
      test "foo bar baz"

    it "" $ do
      test "Foo.bar"

    it "" $ do
      test "Foo.Bar.baz"

    it "" $ do
      test "Foo.Bar.Baz foo"

    it "" $ do
      test "Foo.Bar.Baz"

    it "" $ do
      test "foo.bar.baz"

    it "" $ do
      test "Foo.bar.baz"

    it "" $ do
      test ".foo.bar.baz"

  fdescribe "numeric literals" $ do
    it "" $ do
      test "1"
      test "10"

  fdescribe "stolen syntax" $ do
    it "" $ do
      let input = "Foo. foo"
      map (.tokType) (New.tokenize input) `shouldBe` [IncompleteQualifiedName "Foo.", Identifier "foo"]
      map unLoc (ref input) `shouldBe` [ITconid "Foo", ITdot, ITvarid "foo"]

    it "" $ do
      let input = "Foo."
      map (.tokType) (New.tokenize input) `shouldBe` [IncompleteQualifiedName "Foo."]
      map unLoc (ref input) `shouldBe` [ITconid "Foo", ITdot]
