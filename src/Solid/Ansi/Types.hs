module Solid.Ansi.Types where

import Solid.Common

-- https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
data Ansi a = Ansi [Modifier] a
  deriving (Eq, Show)

data Modifier =
    Bold
  | Faint
  | Underline
  | Inverse
  | Foreground Color
  deriving (Eq, Show)

data Color =
    Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | RGB Word8 Word8 Word8
  deriving (Eq, Show)

ansi :: a -> Ansi a
ansi = Ansi []
