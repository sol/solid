module Syntax (
  highlight

, Group(..)
, annotate
) where

use Haskell
import Solid.Ansi (Color(..))

import Solid.PP (extensions)
import Solid.PP.SrcLoc
import Solid.PP.Lexer (LexerResult, Token(..))
use Solid.PP.Lexer

import Annotated
import Rope (Rope)
use Rope

highlight :: Int -> Int -> Rope -> [Annotated (Maybe Color)]
highlight offset _height = colorize dark . map annotate . Rope.lines . Rope.dropLines offset

data Group =
    Include
  | Structure
  | Statement
  | Operator
  | Comment
  | SpecialComment

  | String
  | Character
  | Number
  | Float
  deriving (Eq, Show)

colorize :: ColorScheme -> [Annotated (Maybe Syntax.Group)] -> [Annotated (Maybe Color)]
colorize scheme = map (fmap (fmap scheme))

type ColorScheme = Syntax.Group -> Color

dark :: ColorScheme
dark = \ case
  Include -> Blue
  Structure -> Green
  Statement -> Yellow
  Operator -> Yellow
  Comment -> Cyan
  SpecialComment -> Magenta
  String -> Magenta
  Character -> Magenta
  Number -> Magenta
  Float -> Magenta

annotate :: String -> Annotated (Maybe Group)
annotate input = Annotated $ case tokenize input of
  Left _ -> [(Nothing, input)]
  Right result -> foo input result.tokens

foo :: String -> [WithBufferSpan Token] -> [(Maybe Group, String)]
foo input = filter (not . String.empty? . snd) . go 0
  where
    go i = \ case
      [] -> [(Nothing, input.drop i)]
      L loc t : xs -> case t of
        ITimport -> link Include
        ITas -> link Include
        (ITvarid "use") -> link Include

        ITmodule -> link Structure
        ITclass -> link Structure
        ITdata -> link Structure
        ITderiving -> link Structure
        ITinstance -> link Structure
        ITdefault -> link Structure
        ITwhere -> link Structure

        ITmdo{} -> link Statement
        ITdo{} -> link Statement
        ITcase -> link Statement
        ITlcase -> link Statement
        ITof -> link Statement
        ITlet -> link Statement
        ITin -> link Statement

        ITvarsym{} -> link Operator
        ITconsym{} -> link Operator
        ITqvarsym{} -> link Operator
        ITqconsym{} -> link Operator

        ITequal -> link Operator
        ITcolon -> link Operator
        ITdcolon{} -> link Operator
        ITrarrow{} -> link Operator
        ITlarrow{} -> link Operator
        ITdot -> link Operator

        ITlineComment{} -> link Comment
        ITblockComment comment _
          | comment.startsWith "\{-#" -> link SpecialComment
          | otherwise -> link Comment

        ITchar{} -> link Character

        ITstring{} -> link String
        ITstring_interpolation_begin{} -> linkSlice loc.start loc.end.pred String
        ITstring_interpolation_end_begin{} -> linkSlice loc.start.succ loc.end.pred String
        ITstring_interpolation_end{} -> linkSlice loc.start.succ loc.end String

        ITinteger{} -> link Number
        ITrational{} -> link Number

        _ -> go i xs
        where
          link = linkSlice loc.start loc.end
          linkSlice start end group = (Nothing, input.slice i start) : (Just group, input.slice start end) : go end xs

tokenize :: String -> Either [Char] LexerResult
tokenize = Lexer.tokenizeWithComments extensions "main.hs" 1 . Haskell.toText
