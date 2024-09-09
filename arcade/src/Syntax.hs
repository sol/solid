module Syntax (
  highlight

, Group(..)
, annotate
, breakOnCharacterEscape
) where

import Data.Function (fix)
import Data.Maybe (mapMaybe, listToMaybe)

use Haskell
import Solid.Ansi (Color(..))

import Solid.PP (language, extensions)
import Solid.PP.SrcLoc
import Solid.PP.Lexer (LexerResult, Token(..))
use Solid.PP.Lexer

import Annotated
import Rope (Rope)
use Rope

use Syntax.Vim

highlight :: Int -> Int -> Rope -> [Annotated (Maybe Color)]
highlight offset _height = colorize dark . map annotate . Rope.lines . Rope.dropLines offset

data Group =
    Comment

  | Constant
  | String
  | Character
  | Number
  | Float

  | Special
  | SpecialChar
  | SpecialComment

  | Statement
  | Conditional
  | Operator
  | Keyword

  | PreProc
  | Include

  | Type
  | Structure
  | Typedef
  deriving (Eq, Show)

colorize :: ColorScheme -> [Annotated (Maybe Syntax.Group)] -> [Annotated (Maybe Color)]
colorize scheme = map (fmap (fmap scheme))

type ColorScheme = Syntax.Group -> Color

dark :: ColorScheme
dark = fix $ \ link -> \ case
  Comment -> Cyan

  Constant -> Magenta
  String -> link Constant
  Character -> link Constant
  Number -> link Constant
  Float -> link Number

  Special -> Vim.LightRed
  SpecialChar -> link Special
  SpecialComment -> link Special

  Statement -> Yellow
  Conditional -> link Statement
  Operator -> link Statement
  Keyword -> link Statement

  PreProc -> Blue
  Include -> link PreProc

  Type -> Green
  Structure -> link Type
  Typedef -> link Type

annotate :: String -> Annotated (Maybe Group)
annotate input = Annotated $ case tokenize input of
  Left _ -> [(Nothing, input)]
  Right result -> annotateInput input result.tokens

annotateInput :: String -> [WithBufferSpan Token] -> [(Maybe Group, String)]
annotateInput input = filter (not . String.empty? . snd) . go 0
  where
    go :: Int -> [WithBufferSpan Token] -> [(Maybe Group, String)]
    go i = \ case
      [] -> [(Nothing, input.drop i)]
      L loc token : tokens -> case token of
        ITdocComment{} -> link Comment
        ITlineComment{} -> link Comment
        ITblockComment comment _
          | comment.startsWith "\{-#" -> link SpecialComment
          | otherwise -> link Comment

        ITinline_prag{} -> link SpecialComment
        ITopaque_prag{} -> link SpecialComment
        ITspec_prag{} -> link SpecialComment
        ITspec_inline_prag{} -> link SpecialComment
        ITsource_prag{} -> link SpecialComment
        ITrules_prag{} -> link SpecialComment
        ITwarning_prag{} -> link SpecialComment
        ITdeprecated_prag{} -> link SpecialComment
        ITline_prag{} -> link SpecialComment
        ITcolumn_prag{} -> link SpecialComment
        ITscc_prag{} -> link SpecialComment
        ITunpack_prag{} -> link SpecialComment
        ITnounpack_prag{} -> link SpecialComment
        ITann_prag{} -> link SpecialComment
        ITcomplete_prag{} -> link SpecialComment
        ITclose_prag{} -> link Type
        IToptions_prag{} -> link SpecialComment
        ITinclude_prag{} -> link SpecialComment
        ITlanguage_prag{} -> link SpecialComment
        ITminimal_prag{} -> link SpecialComment
        IToverlappable_prag{} -> link SpecialComment
        IToverlapping_prag{} -> link SpecialComment
        IToverlaps_prag{} -> link SpecialComment
        ITincoherent_prag{} -> link SpecialComment
        ITctype{} -> link SpecialComment
        ITcomment_line_prag{} -> link SpecialComment

        ITstring{} -> string loc.start loc.end
        ITstring_interpolation_begin{} -> string loc.start loc.end.pred
        ITstring_interpolation_end_begin{} -> string loc.start.succ loc.end.pred
        ITstring_interpolation_end{} -> string loc.start.succ loc.end

        ITchar{} -> link Character
        ITinteger{} -> link Number
        ITrational{} -> link Float

        ITimport -> link Include
        ITqualified -> link Include
        ITas -> link Include
        IThiding -> link Include
        ITvarid "use" -> link Include

        ITforeign -> link Include
        ITstdcallconv -> link Include
        ITccallconv -> link Include
        ITcapiconv -> link Include
        ITprimcallconv -> link Include
        ITjavascriptcallconv -> link Include
        ITsafe -> link Include
        ITunsafe -> link Include
        ITinterruptible -> link Include
        ITexport -> link Include

        ITinfix -> link PreProc
        ITinfixl -> link PreProc
        ITinfixr -> link PreProc

        ITmodule -> link Structure
        ITclass -> link Structure
        ITfamily -> link Structure
        ITdata -> link Structure
        ITderiving -> link Structure
        ITinstance -> link Structure
        ITstock -> link Structure
        ITanyclass -> link Structure
        ITvia -> link Structure

        ITdefault -> link Structure
        ITwhere -> link Structure

        ITtype -> link Typedef
        ITnewtype -> link Typedef
        ITpattern -> link Typedef
        ITrole -> link Typedef

        ITmdo{} -> link Statement
        ITdo{} -> link Statement
        ITcase -> link Statement
        ITlcase -> link Statement
        ITlcases -> link Statement
        ITof -> link Statement
        ITlet -> link Statement
        ITin -> link Statement

        ITif -> link Conditional
        ITthen -> link Conditional
        ITelse -> link Conditional

        ITvarsym{} -> link Operator
        ITconsym{} -> link Operator
        ITqvarsym{} -> link Operator
        ITqconsym{} -> link Operator

        ITdotdot -> link Operator
        ITcolon -> link Operator
        ITdcolon{} -> link Operator
        ITequal -> link Operator
        ITlam -> link Operator
        ITvbar -> link Operator
        ITlarrow{} -> link Operator
        ITrarrow{} -> link Operator
        ITdarrow{} -> link Operator
        ITlolly -> link Operator
        ITminus -> link Operator
        ITprefixminus -> link Operator
        ITbang -> link Operator
        ITtilde -> link Operator
        ITat -> link Operator
        ITtypeApp -> link Operator
        ITpercent -> link Operator
        ITstar{} -> link Operator
        ITdot -> link Operator
        ITproj{} -> skip

        ITstatic -> link Keyword
        ITgroup -> link Keyword
        ITby -> link Keyword
        ITusing -> link Keyword

        ITforall{} -> skip
        ITlabel -> skip
        ITdynamic -> skip

        -- Backpack tokens
        ITunit -> skip
        ITsignature -> skip
        ITdependency -> skip
        ITrequires -> skip

        _ -> skip
        where
          skip = go i tokens

          string :: Int -> Int -> [(Maybe Group, String)]
          string start end = linkSliceWith start end f
            where
              f :: String -> [(Maybe Group, String)] -> [(Maybe Group, String)]
              f s xs = case breakOnCharacterEscape s of
                Nothing -> (Just String, s) : xs
                Just (a, b, c) -> (Just String, a) : (Just SpecialChar, b) : f c xs

          link :: Group -> [(Maybe Group, String)]
          link = linkSlice loc.start loc.end

          linkSlice :: Int -> Int -> Group -> [(Maybe Group, String)]
          linkSlice start end group = linkSliceWith start end $ (,) (Just group) >>> (:)

          linkSliceWith :: Int -> Int -> (String -> [(Maybe Group, String)] -> [(Maybe a, String)]) -> [(Maybe a, String)]
          linkSliceWith start end f = (Nothing, input.slice i start) : f input.slice(start, end) (go end tokens)

breakOnCharacterEscape :: String -> Maybe (String, String, String)
breakOnCharacterEscape input = case input.breakOn "\\" of
  (_, "") -> Nothing
  (xs, ys) -> listToMaybe $ mapMaybe tryPattern patterns
    where
      tryPattern :: String -> Maybe (String, String, String)
      tryPattern p = (,,) xs p <$> ys.stripPrefix p
  where
    patterns = [
        "\\NUL"
      , "\\SOH"
      , "\\STX"
      , "\\ETX"
      , "\\EOT"
      , "\\ENQ"
      , "\\ACK"
      , "\\a"
      , "\\b"
      , "\\t"
      , "\\n"
      , "\\v"
      , "\\f"
      , "\\r"
      , "\\SO"
      , "\\SI"
      , "\\DLE"
      , "\\DC1"
      , "\\DC2"
      , "\\DC3"
      , "\\DC4"
      , "\\NAK"
      , "\\SYN"
      , "\\ETB"
      , "\\CAN"
      , "\\EM"
      , "\\SUB"
      , "\\ESC"
      , "\\FS"
      , "\\GS"
      , "\\RS"
      , "\\US"
      , "\\\""
      , "\\\'"
      , "\\\\"
      , "\\\{"
      ]

tokenize :: String -> Either [Char] LexerResult
tokenize = Lexer.tokenizeWithComments language extensions "main.hs" 1 . Haskell.toText
