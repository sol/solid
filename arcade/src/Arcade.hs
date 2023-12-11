module Arcade where

use System.IO as Haskell

import System.Posix.Terminal.ByteString
import System.Posix.IO.ByteString (stdInput)
import Data.Bits

use Data.Text.Import

data Buffer = Buffer {
  lines :: [String]
, row :: Int
, column :: Int
}

instance HasField "moveUp" Buffer Buffer where
  getField self = self { row = self.row.pred.max 1 }

instance HasField "moveDown" Buffer Buffer where
  getField self = self { row = self.row.succ.min(self.lines.length) }


data Line = EmptyLine | Line Int String | LineContinuation String

renderBuffer :: Int -> Int -> Buffer -> [Line]
renderBuffer width height buffer = take height $ (<> repeat EmptyLine) . concatMap chunks $ List.enumerateFrom buffer.row $ drop buffer.row.pred $ buffer.lines
  where
    chunks :: (Int, String) -> [Line]
    chunks = \ case
      (n, "") -> [Line n ""]
      (n, line) -> List.zipWith ($) (Line n : repeat LineContinuation) Import.chunksOf(width, line) 

openBuffer :: FilePath -> IO Buffer
openBuffer name = do
  contents <- readFile name
  return Buffer {
    lines = contents.lines
  , row = 1
  , column = 1
  }

drawBuffer :: Buffer -> IO ()
drawBuffer buffer = do
  (renderBuffer 50 20 buffer).for_ $ \ line -> do
    case line of
      EmptyLine -> stdout.write "~"
      Line n xs -> stdout.write "{n.toString.rjust(3).ansi.yellow} " >> stdout.write xs
      LineContinuation xs -> stdout.write xs

    -- stdout.write "\ESC[0K\r\n"
    stdout.write "\r\n"

main :: IO ()
main = do
  [file] <- Process.args

  stdin.setEcho False
  stdin.setBuffering IO.NoBuffering
  bracket (getTerminalAttributes stdInput) (flip (setTerminalAttributes stdInput) Immediately) $ \ attributes -> do
    let
      without = flip withoutMode
      attr =
          without KeyboardInterrupts  -- ISIG
        . without StartStopOutput     -- IXON
        . without ExtendedFunctions   -- IEXTEN
        . without MapCRtoLF           -- ICRNL
        . without ProcessOutput       -- OPOST
        -- . without InterruptOnBreak    -- BRKINT
        -- . without CheckParity         -- INPCK
        -- . without StripHighBit        -- ISTRIP
        -- . flip withBits 8             -- CS8
        $ attributes

    setTerminalAttributes stdInput attr Immediately

    buffer <- openBuffer file.asFilePath
    run buffer

ctrl :: Char -> Char
ctrl c = Char.chr $ c.ord .&. 0b00011111

run :: Buffer -> IO ()
run buffer = do
  stdout.write "\ESC[2J"
  stdout.write "\ESC[H"
  drawBuffer buffer
  stdout.write "{buffer.row},{buffer.column}".ansi.inverse.toString
  stdout.write "\r\n"
  processInput

  where
    processInput = do
      Haskell.hGetChar stdin >>= \ case
        'k' -> run buffer.moveUp
        'j' -> run buffer.moveDown
        c | exit? c -> pass
        c -> do
          stdout.write "{c.ord.toString.rjust 3} ({show c})\r\n"
          processInput

    exit? c = c == 'q' || c == ctrl 'q' || c == ctrl 'd'
