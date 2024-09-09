module Arcade where

import Data.Bits ((.&.))

import Terminal
import Annotated (Annotated(..))

use Rope
import Rope (Rope)

use Syntax
use Solid.Ansi

main :: IO ()
main = Process.args >>= \ case
  [file] -> withRawMode stdin . withAlternativeScreenBuffer . withoutCursor $ do
    view <- viewFile 150 50 file.asFilePath
    run view
  _ -> Process.exit "Usage: {} FILE"

run :: View -> IO ()
run view = do
  stdout.write moveCursorHome
  drawView view
  drawStatusLine
  processInput
  where
    drawStatusLine :: IO ()
    drawStatusLine = do
      stdout.write "{view.line},{view.column}".ansi.inverse.toString
      stdout.write newlineErase

    processInput :: IO ()
    processInput = stdin.getChar >>= maybe pass processKeyPress

    processKeyPress :: Char -> IO ()
    processKeyPress c
      | c == 'h' = run view.moveLeft(1)
      | c == 'l' = run view.moveRight(1)
      | c == 'k' = run view.moveUp(1)
      | c == 'j' = run view.moveDown(1)
      | c == ctrl 'y' = run view.scrollUp(1)
      | c == ctrl 'e' = run view.scrollDown(1)
      | c == ctrl 'b' = run view.scrollUp(view.height - 2)
      | c == ctrl 'f' = run view.scrollDown(view.height - 2)
      | exit? c = pass
      | otherwise = do
          stdout.write "{c.ord.toString.rjust 3} ({show c})\r\n"
          processInput

    exit? :: Char -> Bool
    exit? c = c == 'q' || c == ctrl 'q' || c == ctrl 'd'

    ctrl :: Char -> Char
    ctrl c = Char.chr $ c.ord .&. 0b00011111

data View = View {
  buffer :: Rope
, width :: Int
, height :: Int
, offset :: Int
, line :: Int
, column :: Int
}

.currentLine :: View -> String
.currentLine view = case view.buffer.splitAtLine(view.line.pred).snd.lines of
  x : _ -> x
  [] -> ""

.scrollUp :: Int -> View -> View
.scrollUp n view =
  let
    offset = view.offset.minus(n).max(1)
  in
    view {
      offset
    , line = view.line.min(view.height + offset - 1)
    }

.scrollDown :: Int -> View -> View
.scrollDown n view =
  let
    offset = (view.offset + n).min(view.buffer.lengthAsPosition.line.max(1))
  in
    view {
      offset
    , line = view.line.max offset
    }

.moveUp :: Int -> View -> View
.moveUp n view = let
    line = (view.line - n).max 1
  in
    view {
      offset = view.offset.min line
    , line
    }

.moveDown :: Int -> View -> View
.moveDown n view = let
    line = (view.line + n).min view.buffer.lengthAsPosition.line
  in
    view {
      offset = view.offset.max(line - view.height + 1)
    , line
    }

.moveLeft :: Int -> View -> View
.moveLeft n view = let
    column = (view.column - n).max 1
  in
    view {
      column
    }

.moveRight :: Int -> View -> View
.moveRight n view = let
    column = (view.column + n).min view.currentLine.length
  in
    view {
      column
    }

viewFile :: Int -> Int -> FilePath -> IO View
viewFile width height name = do
  buffer <- Rope.fromString <$> readFile name
  return View {
    buffer
  , width
  , height
  , offset = 1
  , line = 1
  , column = 1
  }

data Line a = EmptyLine | Line Int a | LineContinuation a
  deriving (Eq, Show, Functor)

renderView :: View -> [Line (Annotated (Maybe Ansi.Color))]
renderView view =
    Syntax.highlight view.offset.pred view.height view.buffer
  & splitLines
  & take view.height
  where
    splitLines :: [Annotated (Maybe Ansi.Color)] -> [Line (Annotated (Maybe Ansi.Color))]
    splitLines =
          List.enumerateFrom view.offset
      >>> concatMap splitLine
      >>> (<> repeat EmptyLine)

    splitLine :: (Int, Annotated (Maybe Ansi.Color)) -> [Line (Annotated (Maybe Ansi.Color))]
    splitLine (n, line) = List.zipWith ($) (Line n : repeat LineContinuation) chunks
      where
        chunks = case line.chunksOf view.width of
          [] -> [line]
          xs -> xs

drawView :: View -> IO ()
drawView view = (renderView view).foreach $ \ line -> do
  case line of

    Line n xs | n == view.line -> do
      stdout.write "{n.toString.rjust(3).ansi.yellow} "

      let
        (foo, bar_) = xs.splitAt view.column.pred


      printBufferLine foo
      case bar_.splitAt 1 of
        (Annotated [], _) -> do
          stdout.write "{(String.ansi(" ").inverse)}"

        (Annotated c, bar) -> do
          c.for_ $ \ (_, cc) -> do
            stdout.write "{cc.ansi.inverse}"
          printBufferLine bar

    Line n xs -> stdout.write "{n.toString.rjust(3).ansi.yellow} " >> printBufferLine xs -- DRY up with previous case
    LineContinuation xs -> stdout.write "    " >> printBufferLine xs
    EmptyLine -> stdout.write "~"
  stdout.write newlineErase
  where
    printBufferLine :: Annotated (Maybe Ansi.Color) -> IO ()
    printBufferLine (Annotated chunks) =
      chunks.foreach $ stdout.write . uncurry (maybe id colorize)

    colorize :: Ansi.Color -> String -> String
    colorize color = String.ansi >>> Ansi.foreground color >>> toString

moveCursorHome :: String
moveCursorHome = "\ESC[H"

newlineErase :: String
newlineErase = "\ESC[K\r\n"
