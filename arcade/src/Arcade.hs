module Arcade (main) where

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
  _ -> Process.exit "Usage: \{} FILE"

run :: View -> IO ()
run view = do
  stdout.write moveCursorHome
  drawView view
  drawStatusLine
  processInput
  where
    drawStatusLine :: IO ()
    drawStatusLine = do
      stdout.write "\{view.line},\{view.column}".ansi.inverse.toString
      stdout.write newlineErase

    processInput :: IO ()
    processInput = stdin.getChar >>= maybe pass processKeyPress

    processKeyPress :: Char -> IO ()
    processKeyPress c
      | c == 'k' = run view.moveUp(1)
      | c == 'j' = run view.moveDown(1)
      | c == ctrl 'y' = run view.moveUp(1)
      | c == ctrl 'e' = run view.moveDown(1)
      | c == ctrl 'b' = run view.moveUp(view.height - 2)
      | c == ctrl 'f' = run view.moveDown(view.height - 2)
      | exit? c = pass
      | otherwise = do
          stdout.write "\{c.ord.toString.rjust 3} (\{show c})\r\n"
          processInput

    exit? :: Char -> Bool
    exit? c = c == 'q' || c == ctrl 'q' || c == ctrl 'd'

    ctrl :: Char -> Char
    ctrl c = Char.chr $ c.ord .&. 0b00011111

data View = View {
  buffer :: Rope
, width :: Int
, height :: Int
, line :: Int
, column :: Int
}

instance HasField "moveUp" View (Int -> View) where
  getField self n = self { line = (self.line - n).max 1 }

instance HasField "moveDown" View (Int -> View) where
  getField self n = self { line = (self.line + n).min self.buffer.lengthAsPosition.line }

viewFile :: Int -> Int -> FilePath -> IO View
viewFile width height name = do
  buffer <- Rope.fromString <$> readFile name
  return View {
    buffer
  , width
  , height
  , line = 1
  , column = 1
  }

data Line a = EmptyLine | Line Int a | LineContinuation a
  deriving (Eq, Show, Functor)

renderView :: View -> [Line (Annotated (Maybe Ansi.Color))]
renderView view =
    Syntax.highlight view.line.pred view.height view.buffer
  & splitLines
  & take view.height
  where
    splitLines :: [Annotated (Maybe Ansi.Color)] -> [Line (Annotated (Maybe Ansi.Color))]
    splitLines =
          List.enumerateFrom view.line
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
    Line n xs -> stdout.write "\{n.toString.rjust(3).ansi.yellow} " >> printBufferLine xs
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
