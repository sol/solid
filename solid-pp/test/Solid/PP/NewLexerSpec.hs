{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Solid.PP.NewLexerSpec (spec) where

import           Prelude ()
import           Solid.PP.IO hiding (mod, read)

import           Test.Hspec

import           Data.Text.Internal(Text(..))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Array as Array
import           Data.ByteString.Internal (c2w)
import           Text.Read (readMaybe)
import           GHC.Stack (withFrozenCallStack)
import           GHC.Parser.Annotation (IsUnicodeSyntax(..))

import           Solid.PP (language, extensions)
import qualified Solid.PP.Lexer as Old
import           Solid.PP.Lexer hiding (toBufferSpan, tokenize)

import           Solid.PP.NewLexer (TokenType(..))
import qualified Solid.PP.NewLexer as New

ref :: HasCallStack => Text -> [WithBufferSpan Token]
ref = a_FIXME_DiscardLayoutForNow . either error ((.tokens)) . Old.tokenize language extensions "" 1
  where
    a_FIXME_DiscardLayoutForNow :: [WithBufferSpan Token] -> [WithBufferSpan Token]
    a_FIXME_DiscardLayoutForNow = filter $ unLoc >>> \ case
      ITvocurly -> False
      ITvccurly -> False
      ITsemi -> False
      _ -> True

tokenizeFoo :: Text -> [WithBufferSpan Token]
tokenizeFoo input = adjustProjectionFixities . toTokens input . New.synthesize . New.tokenize $ input

tokenize :: Text -> [(TokenType, Text)]
tokenize input = map ((.tokenType) &&& New.textSpan input) (New.synthesize $ New.tokenize input)


toBufferSpan :: New.Span -> BufferSpan
toBufferSpan loc = BufferSpan "" start.charOffset end.charOffset start.line end.line start.column end.column
  where
    start = loc.start
    end = loc.end

toTokens :: Text -> [New.Token] -> [WithBufferSpan Token]
toTokens input = loop
  where
    foo :: Int -> Int -> FastString
    foo start = toFastString . New.textSpan_ input start

    l :: New.Span -> New.Span -> e -> GenLocated BufferSpan e
    l start end = L (toBufferSpan $ New.Span start.start end.end)

    loop :: [New.Token] -> [WithBufferSpan Token]
    loop = \ case
      [] -> []
      New.Token Projection span : rest -> L (toBufferSpan $ New.Span start end) (ITproj True) : name : loop rest
        where
          start = span.start

          end :: New.Location
          end = New.adjustOffset 1 span.start

          nameSpan :: New.Span
          nameSpan = New.Span end span.end

          name = L (toBufferSpan nameSpan) . ITvarid . toFastString $ New.textSpan__ input nameSpan
      token : rest -> toToken token : loop rest

    toToken :: New.Token -> WithBufferSpan Token
    toToken token = L (toBufferSpan token.span) case token.tokenType of

      Identifier
        | fs == "module" -> ITmodule
        | fs == "where" -> ITwhere
        | fs == "import" -> ITimport
        | fs == "hiding" -> IThiding
        | fs == "as" -> ITas
        | fs == "qualified" -> ITqualified
        | fs == "data" -> ITdata
        | fs == "deriving" -> ITderiving
        | otherwise -> ITvarid fs

      Constructor -> ITconid fs
      QualifiedIdentifier -> ITqvarid name
      QualifiedConstructor -> ITqconid name

      Symbol _
        | fs == "." -> ITdot
        | fs == "!" -> ITbang

        | fs == ".." -> ITdotdot
        | fs == ":" -> ITcolon
        | fs == "::" -> ITdcolon NormalSyntax
        | fs == "=" -> ITequal
        | fs == "\\" -> ITlam
        | fs == "|" -> ITvbar
        | fs == "<-" -> ITlarrow NormalSyntax
        | fs == "->" -> ITrarrow NormalSyntax
        -- | fs == "@" -> ITat
        -- | fs == "~" -> ITtilde
        | fs == "=>" -> ITdarrow NormalSyntax

        | otherwise -> ITvarsym fs

      Integer -> ITinteger (IL sourceText False $ read (unpack text)) -- FIXME: read
      String -> ITstring sourceText $ mkFastString (read (unpack text)) -- FIXME: read

      Special '(' -> IToparen
      Special ')' -> ITcparen
      Special '{' -> ITocurly
      Special '}' -> ITccurly
      Special ',' -> ITcomma

      Special _char -> ITeof

      Comment -> ITeof
      EndOfFile -> ITeof
      _ -> ITeof
      where
        fs = toFastString text

        text :: Text
        text = New.textSpan input token

        name :: (FastString, FastString)
        name = (toFastString $ Text arr off (dot - off), toFastString $ Text arr (dot + 1) (len + off - dot - 1))
          where
            Text arr off len = text

            dot = findDot (off + len - 1)

            findDot !i
              | Array.unsafeIndex arr i == c2w '.' = i
              | otherwise = findDot (i - 1)

        sourceText :: SourceText
        sourceText = SourceText (toFastString text)

-- read :: HasCallStack => Read a => String -> a
read :: Read a => String -> a
read input = case readMaybe input of
  Nothing -> withFrozenCallStack $ error $ "could not parse " <> show input
  Just a -> a

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
  tokenizeFoo input `shouldBe` ref input
  where
    actual = tokenizeFoo input
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
      let
        skip = 39
        line = 40

      input <- take (line - skip) . drop skip . Text.lines <$> Text.readFile "src/Solid/PP/NewLexer.hs"

      test $ Text.unlines input

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

    it "" $ do
      let input = "\"foo"
      tokenize input `shouldBe` [(UnterminatedString, "\"foo")]

    it "" $ do
      let input = "\"foo\nbar"
      tokenize input `shouldBe` [(UnterminatedString, "\"foo"), (Identifier, "bar")]

  describe "projection" $ do
    it "" $ do
      let input = ".foo"
      tokenize input `shouldBe` [(Projection, ".foo")]
      map unLoc (ref input) `shouldBe` [ITproj True, ITvarid "foo"]

  describe "reservedop" $ do
    it "" $ do
      let input = ".. : :: = \\ | <- -> @ ~ =>"
      test input

  describe "stolen syntax" $ do
    it "" $ do
      let input = "Foo. foo"
      tokenize input `shouldBe` [(IncompleteQualifiedName, "Foo."), (Identifier, "foo")]
      map unLoc (ref input) `shouldBe` [ITconid "Foo", ITdot, ITvarid "foo"]

    it "" $ do
      let input = "Foo."
      tokenize input `shouldBe` [(IncompleteQualifiedName, "Foo.")]
      map unLoc (ref input) `shouldBe` [ITconid "Foo", ITdot]

    it "single line comments must start with exactly --" $ do
      let input = "---"
      tokenize input `shouldBe` [(Symbol "---", "---")]
      map unLoc (ref input) `shouldBe` []

  describe "index" $ do
    it "" do
      let
        t = "foobar"
      New.index t 5 `shouldBe` 'r'
      New.index t 6 `shouldBe` '\0'

      New.index (Text.drop 1 t) 4 `shouldBe` 'r'
      New.index (Text.drop 1 t) 5 `shouldBe` '\0'
