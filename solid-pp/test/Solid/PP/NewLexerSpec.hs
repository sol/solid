{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Solid.PP.NewLexerSpec (spec) where

import           Prelude ()
import           Solid.PP.IO hiding (mod)

import           Test.Hspec

import           Solid.PP (language, extensions)
import qualified Solid.PP.Lexer as Old
import           Solid.PP.Lexer hiding (toBufferSpan, tokenize)

import           Solid.PP.NewLexer (Tok(..), TokenType(..))
import qualified Solid.PP.NewLexer as New

ref :: HasCallStack => Text -> [WithBufferSpan Token]
ref = either error ((.tokens)) . Old.tokenize language extensions "" 1

tokenize :: Text -> [WithBufferSpan Token]
tokenize input = adjustProjectionFixities . toTokens input . New.tokenize $ input

toBufferSpan :: New.SrcSpan -> BufferSpan
toBufferSpan loc = BufferSpan "" start.charOffset end.charOffset start.line end.line start.column end.column
  where
    start = loc.start
    end = loc.end

toTokens :: Text -> [New.Tok] -> [WithBufferSpan Token]
toTokens input = loop
  where
    foo :: Int -> Int -> FastString
    foo start = toFastString . New.textSpan_ input start

    l :: New.SrcSpan -> New.SrcSpan -> e -> GenLocated BufferSpan e
    l start end = L (toBufferSpan $ New.SrcSpan start.start end.end)

    loop :: [Tok] -> [WithBufferSpan Token]
    loop = \ case
      [] -> []
      Tok Constructor start : Tok (Operator ".") loc : rest | start.end.offset == loc.start.offset -> qualifiedName start rest
      Tok (Operator ".") dot : token@(Tok Identifier ident) : rest | dot.end.offset == ident.start.offset
        -> L (toBufferSpan dot) (ITproj True) : loop (token : rest)
      token : rest -> toToken token : loop rest

    qualifiedName :: New.SrcSpan -> [Tok] -> [WithBufferSpan Token]
    qualifiedName start = go
      where
        xx end = (
            foo start.start.offset (end.start.offset - 1)
          , toFastString $ New.textSpan__ input end
          )


        go = \ case
          Tok Constructor _ : Tok (Operator ".") loc : rest -> qualifiedName start rest

          Tok t end : rest -> case t of
            Constructor -> accept ITqconid
            Identifier -> accept ITqvarid
            _ -> undefined
            where
              accept c = l start end (c (xx end)) : loop rest
          _ -> undefined

          -- Tok (Constructor name) end : rest -> L (toBufferSpan $ New.SrcSpan start.start end.end) (ITqconid (foo start.start.offset (end.start.offset - 1), toFastString name)) : loop rest

    toToken :: Tok -> WithBufferSpan Token
    toToken token = L (toBufferSpan token.span) case token.tokenType of
      Keyword -> ITeof
      Identifier -> ITvarid (toFastString text)
      QualifiedIdentifier mod name -> ITqvarid (toFastString mod, toFastString name)
      Constructor -> ITconid (toFastString text)
      QualifiedConstructor mod name -> ITqconid (toFastString mod, toFastString name)
      IncompleteQualifiedName _ -> ITeof
      Operator "." -> ITdot
      Operator _text -> ITeof
      Integer -> ITinteger (IL sourceText False $ read (unpack text)) -- FIXME: read
      String -> ITstring sourceText $ mkFastString (read (unpack text)) -- FIXME: read
      Symbol _char -> ITeof
      Comment -> ITeof
      EndOfFile -> ITeof
      where
        text :: Text
        text = New.textSpan input token

        sourceText :: SourceText
        sourceText = SourceText (toFastString text)

toFastString :: Text -> FastString
toFastString = unpack >>> fromString -- FIXME

adjustProjectionFixities :: [WithBufferSpan Token] -> [WithBufferSpan Token]
adjustProjectionFixities = loop
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
spec = focus do
{-
  describe "foobar" $ do
    fit "accepts identifiers" $ do
      New.foobar "Foo.Bar.baz more input" `shouldBe` ("Foo.Bar", "baz", " more input")
      -}

  describe "tokenize" $ do
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

    it "" $ do
      test "Foo . bar"

  describe "numeric literals" $ do
    it "" $ do
      test "1"
      test "10"

  describe "string literals" $ do
    it "" $ do
      let
        input = pack $ show @String "foo"
        tokens = New.tokenize input
      New.textSpan input <$> tokens `shouldBe` [input]
      (.tokenType) <$> tokens `shouldBe` [String]
      test input

    it "" $ do
      let
        input = pack $ show @String "foo\"bar"
        tokens = New.tokenize input
      New.textSpan input <$> tokens `shouldBe` [input]
      (.tokenType) <$> tokens `shouldBe` [String]
      test input

    it "" $ do
      test "\"foo\\\"\""

  describe "stolen syntax" $ do
    xit "" $ do
      let input = "Foo. foo"
      map (.tokenType) (New.tokenize input) `shouldBe` [IncompleteQualifiedName "Foo.", Identifier]
      map unLoc (ref input) `shouldBe` [ITconid "Foo", ITdot, ITvarid "foo"]

    xit "" $ do
      let input = "Foo."
      map (.tokenType) (New.tokenize input) `shouldBe` [IncompleteQualifiedName "Foo."]
      map unLoc (ref input) `shouldBe` [ITconid "Foo", ITdot]
