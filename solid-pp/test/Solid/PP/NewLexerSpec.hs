{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Solid.PP.NewLexerSpec (spec) where

import Data.Text.IO.Utf8 qualified as Utf8
import Data.Text.Internal.Encoding.Utf8 (utf8Length)
import           Prelude ()
import           Solid.PP.IO hiding (mod, span, read)

import           Test.Hspec

import           Data.Text.Internal(Text(..))
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
import           Solid.PP.LocationIndex

ref :: HasCallStack => Text -> [WithBufferSpan Token]
ref = a_FIXME_Discard_UNPACK_ForNow . a_FIXME_Discard_INLINE_ForNow . a_FIXME_DiscardLayoutForNow . either error ((.tokens)) . Old.tokenize language extensions "" 1
  where
    a_FIXME_DiscardLayoutForNow :: [WithBufferSpan Token] -> [WithBufferSpan Token]
    a_FIXME_DiscardLayoutForNow = filter $ unLoc >>> \ case
      ITvocurly -> False
      ITvccurly -> False
      ITsemi -> False
      _ -> True

    a_FIXME_Discard_INLINE_ForNow :: [WithBufferSpan Token] -> [WithBufferSpan Token]
    a_FIXME_Discard_INLINE_ForNow = go
      where
        go = \ case
          [] -> []
          L _ (ITinline_prag _ _ _) : xs -> go $ drop 1 $ dropWhile isITclose_prag xs
          x : xs -> x : go xs

        isITclose_prag = (unLoc >>> not . (== ITclose_prag))

    a_FIXME_Discard_UNPACK_ForNow :: [WithBufferSpan Token] -> [WithBufferSpan Token]
    a_FIXME_Discard_UNPACK_ForNow = go
      where
        go = \ case
          [] -> []
          L _ (ITunpack_prag _) : xs -> go $ drop 1 $ dropWhile isITclose_prag xs
          x : xs -> x : go xs

        isITclose_prag = (unLoc >>> not . (== ITclose_prag))

discardPragmas :: [New.Token] -> [New.Token]
discardPragmas = filter \ case
  New.Token Pragma _ -> False
  _ -> True

tokenizeFoo :: Bool -> Text -> [WithBufferSpan Token]
tokenizeFoo patternSynonymsEnabled input = adjustProjectionFixities . toTokens patternSynonymsEnabled input . discardPragmas . New.synthesize input . New.tokenize $ input

tokenize :: Text -> [(TokenType, Text)]
tokenize input = map ((.tokenType) &&& New.tokenText input) (New.synthesize input $ New.tokenize input)


xxx :: Text -> [Int]
xxx = go 0 . unpack
  where
    go n = \ case
      [] -> [n]
      x : xs -> replicate (utf8Length x) n ++ go (n + 1) xs
      {-
      x : xs -> case utf8Length x of
        1 -> n : go (n + 1) xs
        2 -> n : n : go (n + 1) xs
        3 -> n : n : n : go (n + 1) xs
        _ -> n : n : n : n : go (n + 1) xs
        -}


toTokens :: Bool -> Text -> [New.Token] -> [WithBufferSpan Token]
toTokens patternSynonymsEnabled input = loop
  where
    yyy = xxx input

    index :: LocationIndex
    index = locationIndex input

    toBufferSpan :: New.Span -> BufferSpan
    toBufferSpan loc = BufferSpan "" (charOffset start) (charOffset end) startLine endLine startColumn endColumn
      where
        startLine = offsetLine start index
        endLine = offsetLine (end - 1) index

        startColumn = offsetColumn start index
        endColumn = offsetColumn end index

        start = loc.start
        end = loc.end

        charOffset n = yyy !! n


    loop :: [New.Token] -> [WithBufferSpan Token]
    loop = \ case
      [] -> []
      New.Token Projection span : rest -> L (toBufferSpan $ New.Span start end) (ITproj True) : name : loop rest
        where
          start :: Int
          start = span.start

          end :: Int
          end = span.start + 1

          nameSpan :: New.Span
          nameSpan = New.Span end span.end

          name :: WithBufferSpan Token
          name = L (toBufferSpan nameSpan) . ITvarid . toFastString $ New.textSpan input nameSpan

      token : rest -> toToken token : loop rest

    _keywords :: [Text]
    _keywords =
      [ "case", "class", "data", "default", "deriving", "do", "else", "if"
      , "import", "in", "infix", "infixl", "infixr", "instance", "let"
      , "module", "newtype", "of", "then", "type", "where", "forall"
      ]

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
        | fs == "case" -> ITcase
        | fs == "of" -> ITof
        | fs == "type" -> ITtype
        | fs == "newtype" -> ITnewtype
        | fs == "instance" -> ITinstance
        | fs == "let" -> ITlet
        | fs == "in" -> ITin
        | fs == "if" -> ITif
        | fs == "then" -> ITthen
        | fs == "else" -> ITelse
        | fs == "do" -> ITdo Nothing -- FIXME: qualified do
        | fs == "_" -> ITunderscore

        | patternSynonymsEnabled && fs == "pattern" -> ITpattern

        | otherwise -> ITvarid fs

      Constructor -> ITconid fs
      QualifiedIdentifier -> ITqvarid name
      QualifiedConstructor -> ITqconid name


      -- .. | : | :: | = | \ | | | <- | -> | @ | ~ | =>
      -- reservedop =  ["..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]
      Symbol
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
      Char -> ITchar sourceText $ (read (unpack text)) -- FIXME: read
      String -> ITstring sourceText $ mkFastString (read (unpack text)) -- FIXME: read

      Comma -> ITcomma
      Semicolon -> ITsemi
      ParenOpen -> IToparen
      ParenClose -> ITcparen
      BracketOpen -> ITobrack
      BracketClose -> ITcbrack
      BraceOpen -> ITocurly
      BraceClose -> ITccurly


      Pragma -> undefined -- ITinline_prag sourceText (Inline sourceText) FunLike
      Backtick -> ITbackquote

      Comment -> ITeof
      EndOfFile -> ITeof
      _ -> ITeof
      where
        fs = toFastString text

        text :: Text
        text = New.tokenText input token

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

      L loc_lam ITlam : L loc_case ITcase : tokens -> L loc_lam ITlam : L loc_case ITlcase : loop tokens

      -- foo@True
      token : L at (ITvarsym "@") : tokens | (getLoc token).end == at.start ->
        token : L at ITat : loop tokens

      -- show @String
      L at (ITvarsym "@") : token : tokens | at.end == (getLoc token).start ->
        L at ITtypeApp : token : loop tokens

      token@(L _ IToparen) : L loc (ITproj True) : tokens -> token : L loc (ITproj True) : loop tokens
      token : L loc (ITproj True) : tokens | (getLoc token).end == loc.start -> token : L loc (ITproj False) : loop tokens

      token : tokens -> token : loop tokens



test :: HasCallStack => Text -> Expectation
test = testWith False

testWith :: Bool -> HasCallStack => Text -> Expectation
testWith patternSynonymsEnabled input = do
  map unLoc actual `shouldBe` map unLoc expected
  actual `shouldBe` expected
  where
    actual :: [WithBufferSpan Token]
    actual = tokenizeFoo patternSynonymsEnabled input

    expected :: [WithBufferSpan Token]
    expected = ref input

spec :: Spec
spec = focus do
{-
  describe "foobar" do
    fit "accepts identifiers" do
      New.foobar "Foo.Bar.baz more input" `shouldBe` ("Foo.Bar", "baz", " more input")
      -}

  describe "tokenize" do
    it "" do
      input <- Utf8.readFile "src/Solid/PP/NewLexer.hs"
      test input

    it "" do
      input <- Utf8.readFile "src/Solid/PP/NewLexer/Type.hs"
      test input

    it "" do
      input <- Utf8.readFile "src/Solid/PP/Lexer.hs"
      testWith True input

    xit "" do
      input <- Utf8.readFile "test/Solid/PP/NewLexerSpec.hs"
      test input

    context "case" do
      it "" do
        test "foo = case x of"

      it "" do
        test "foo = \\ case"

    it "" do
      test "`"

    it "" do
      test "foo"

    it "" do
      test "foo'bar"

    it "" do
      test "_foo"

    it "" do
      test "_"

    it "" do
      test "fλλ"

    it "" do
      test "a我超爱中国菜 -- ooo"

    it "" do
      test "_foo"

    it "" do
      test "Foo"

    it "" do
      test "Foo foo"

    it "" do
      test "foo bar baz"

    it "" do
      test "Foo.bar"

    it "" do
      test "Foo.Bar.baz"

    it "" do
      test "Foo.Bar.Baz foo"

    it "" do
      test "Foo.Bar.Baz"

    it "" do
      test "foo.bar.baz"

    it "" do
      test "Foo.bar.baz"

    it "" do
      test ".foo.bar.baz"

    it "" do
      test "Foo . bar"

  describe "the @" do
    it "" do
      let
        input = "@"
      tokenize input `shouldBe` [(Symbol, "@")]
      test input

    it "" do
      let
        input = "show @String"
        -- tokens = New.tokenize input
      -- New.textSpan input <$> tokens `shouldBe` ["@"]
      -- (.tokenType) <$> tokens `shouldBe` [Symbol "@"]
      test input

    it "" do
      let
        input = "foo@(Just n)"
        -- tokens = New.tokenize input
      -- New.textSpan input <$> tokens `shouldBe` ["@"]
      -- (.tokenType) <$> tokens `shouldBe` [Symbol "@"]
      test input

    xit "" do
      let
        input = "foo@ (Just n)"
        -- tokens = New.tokenize input
      -- New.textSpan input <$> tokens `shouldBe` ["@"]
      -- (.tokenType) <$> tokens `shouldBe` [Symbol "@"]
      test input

  describe "numeric literals" do
    it "" do
      test "1"
      test "10"

  describe "INLINE pragmas" do
    it "" do
      let
        input = "{-#  INLINE foo #-}"
        tokens = New.tokenize input

      (.tokenType) <$> tokens `shouldBe` [Pragma]
      -- test input

    it "" do
      let
        input = "{-# INLINE foo "
        tokens = New.tokenize input

      (.tokenType) <$> tokens `shouldBe` [UnterminatedPragma]
      -- test input

  describe "comments" do
    it "" do
      let
        input = "foo -- bar\n23"
        tokens = New.tokenize input

      New.tokenText input <$> tokens `shouldBe` ["foo", "23"]
      (.tokenType) <$> tokens `shouldBe` [Identifier, Integer]
      test input

    it "" do
      let
        input = "foo {- bar }- baz -} 23"
        tokens = New.tokenize input
      New.tokenText input <$> tokens `shouldBe` ["foo", "23"]
      (.tokenType) <$> tokens `shouldBe` [Identifier, Integer]
      test input

    it "" do
      let
        input = "23{-foo{-bar-}baz-}42"
        tokens = New.tokenize input
      New.tokenText input <$> tokens `shouldBe` ["23", "42"]
      (.tokenType) <$> tokens `shouldBe` [Integer, Integer]
      test input

    it "" do
      let
        input = "{-foo{-bar-}baz-}" -- FIXME: test all possible prefixes
        tokens = New.tokenize input
      New.tokenText input <$> tokens `shouldBe` []
      (.tokenType) <$> tokens `shouldBe` []

    it "" do
      let
        input = "{- foo"
        tokens = New.tokenize input
      New.tokenText input <$> tokens `shouldBe` []
      (.tokenType) <$> tokens `shouldBe` []

  describe "pragmas" do
    it "" do
      let
        input = "{-# INLINE foo #-}"
        tokens = New.tokenize input
      New.tokenText input <$> tokens `shouldBe` ["{-# INLINE foo #-}"]
      (.tokenType) <$> tokens `shouldBe` [Pragma]

  describe "character literals" do
    it "" do
      test "'a'"

    xit "" do
      test "'f' : 'λ' 'λ' : []"
      -- test . pack $ show @String "我超爱中国菜！"

  describe "string literals" do
    it "" do
      let
        input = pack $ show @String "foo"
        tokens = New.tokenize input
      New.tokenText input <$> tokens `shouldBe` [input]
      (.tokenType) <$> tokens `shouldBe` [String]
      test input

    it "accepts Unicode strings" do
      test . pack $ show @String "fλλ"

    it "accepts Unicode strings" do
      test . pack $ show @String "我超爱中国菜！"

    it "" do
      let
        input = pack $ show @String "foo\"bar"
        tokens = New.tokenize input
      New.tokenText input <$> tokens `shouldBe` [input]
      (.tokenType) <$> tokens `shouldBe` [String]
      test input

    it "" do
      test "\"foo\\\"\""

    it "" do
      let input = "\"foo"
      tokenize input `shouldBe` [(UnterminatedString, "\"foo")]

    it "" do
      let input = "\"foo\nbar"
      tokenize input `shouldBe` [(UnterminatedString, "\"foo"), (Identifier, "bar")]

    it "" do
      let input = "\"foo\\"
      tokenize input `shouldBe` [(UnterminatedString, "\"foo\\")]

  describe "projection" do
    it "" do
      let input = ".foo"
      tokenize input `shouldBe` [(Projection, ".foo")]
      map unLoc (ref input) `shouldBe` [ITproj True, ITvarid "foo"]
      test input

    it "" do
      let input = "(.foo)"
      tokenize input `shouldBe` [(ParenOpen, "("), (Projection, ".foo"), (ParenClose, ")")]
      map unLoc (ref input) `shouldBe` [IToparen, ITproj True, ITvarid "foo", ITcparen]
      test input

  describe "special" do
    xit "semicolon" do
      test "{ foo ; bar }"

  describe "contextual keywords" do
    it "pattern" do
      test "pattern"

  describe "reservedop" do
    it "" do
      let input = ".. : :: = \\ | <- -> @ ~ =>"
      test input

    it "" do
      test "x:xs"

  describe "stolen syntax" do
    it "" do
      let input = "Foo. foo"
      tokenize input `shouldBe` [(IncompleteQualifiedName, "Foo."), (Identifier, "foo")]
      map unLoc (ref input) `shouldBe` [ITconid "Foo", ITdot, ITvarid "foo"]

    it "" do
      let input = "Foo."
      tokenize input `shouldBe` [(IncompleteQualifiedName, "Foo.")]
      map unLoc (ref input) `shouldBe` [ITconid "Foo", ITdot]

    it "single line comments must start with exactly --" do
      let input = "---"
      tokenize input `shouldBe` [(Symbol, "---")]
      map unLoc (ref input) `shouldBe` []
