{-# LANGUAGE PatternSynonyms #-}
module Syntax.Vim where

use Solid.Ansi

pattern Black :: Ansi.Color
pattern Black = Ansi.PaletteColor 0

pattern DarkBlue :: Ansi.Color
pattern DarkBlue = Ansi.PaletteColor 4

pattern DarkGreen :: Ansi.Color
pattern DarkGreen = Ansi.PaletteColor 2

pattern DarkCyan :: Ansi.Color
pattern DarkCyan = Ansi.PaletteColor 6

pattern DarkRed :: Ansi.Color
pattern DarkRed = Ansi.PaletteColor 1

pattern DarkMagenta :: Ansi.Color
pattern DarkMagenta = Ansi.PaletteColor 5

pattern Brown :: Ansi.Color
pattern Brown = Ansi.PaletteColor 130

pattern DarkYellow :: Ansi.Color
pattern DarkYellow = Ansi.PaletteColor 3

pattern Gray :: Ansi.Color
pattern Gray = Ansi.PaletteColor 248

pattern Grey :: Ansi.Color
pattern Grey = Ansi.PaletteColor 248

pattern LightGray :: Ansi.Color
pattern LightGray = Ansi.PaletteColor 7

pattern LightGrey :: Ansi.Color
pattern LightGrey = Ansi.PaletteColor 7

pattern DarkGray :: Ansi.Color
pattern DarkGray = Ansi.PaletteColor 242

pattern DarkGrey :: Ansi.Color
pattern DarkGrey = Ansi.PaletteColor 242

pattern Blue :: Ansi.Color
pattern Blue = Ansi.PaletteColor 12

pattern LightBlue :: Ansi.Color
pattern LightBlue = Ansi.PaletteColor 81

pattern Green :: Ansi.Color
pattern Green = Ansi.PaletteColor 10

pattern LightGreen :: Ansi.Color
pattern LightGreen = Ansi.PaletteColor 121

pattern Cyan :: Ansi.Color
pattern Cyan = Ansi.PaletteColor 14

pattern LightCyan :: Ansi.Color
pattern LightCyan = Ansi.PaletteColor 159

pattern Red :: Ansi.Color
pattern Red = Ansi.PaletteColor 9

pattern LightRed :: Ansi.Color
pattern LightRed = Ansi.PaletteColor 224

pattern Magenta :: Ansi.Color
pattern Magenta = Ansi.PaletteColor 13

pattern LightMagenta :: Ansi.Color
pattern LightMagenta = Ansi.PaletteColor 225

pattern Yellow :: Ansi.Color
pattern Yellow = Ansi.PaletteColor 11

pattern LightYellow :: Ansi.Color
pattern LightYellow = Ansi.PaletteColor 229

pattern White :: Ansi.Color
pattern White = Ansi.PaletteColor 15
