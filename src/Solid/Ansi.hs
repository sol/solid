{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Solid.Ansi (
  ansi

, bold
, underline
, inverse

, black
, red
, green
, yellow
, blue
, magenta
, cyan
, white
, rgb

, on_black
, on_red
, on_green
, on_yellow
, on_blue
, on_magenta
, on_cyan
, on_white
, on_rgb

, Modifier(..)
) where

import Solid.Common
import Solid.ToString as ToString
import List ()
import String (String)
import FilePath (FilePath)

import Solid.Ansi.Types

modifier :: Modifier -> Ansi a -> Ansi a
modifier m (Ansi ms a) = Ansi (m : ms) a

foreground :: Color -> Ansi a -> Ansi a
foreground = modifier . Foreground

background :: Color -> Ansi a -> Ansi a
background = modifier . Background

modifierSet :: Modifier -> String
modifierSet = \ case
  Bold -> "1"
  Faint -> "2"
  Underline -> "4"
  Inverse -> "7"

  Foreground Black -> "30"
  Foreground Red -> "31"
  Foreground Green -> "32"
  Foreground Yellow -> "33"
  Foreground Blue -> "34"
  Foreground Magenta -> "35"
  Foreground Cyan -> "36"
  Foreground White -> "37"
  Foreground (RGB r g b) -> "38;2;{r};{g};{b}"

  Background Black -> "40"
  Background Red -> "41"
  Background Green -> "42"
  Background Yellow -> "43"
  Background Blue -> "44"
  Background Magenta -> "45"
  Background Cyan -> "46"
  Background White -> "47"
  Background (RGB r g b) -> "48;2;{r};{g};{b}"

instance ToString Modifier where
  toString m = "\ESC[{modifierSet m}m"

modifierUnset :: Modifier -> String
modifierUnset = \ case
  Bold -> "22"
  Faint -> "22"
  Underline -> "24"
  Inverse -> "27"
  Foreground _ -> "39"
  Background _ -> "49"

instance HasField "set" Modifier String where
  getField m = "\ESC[{modifierSet m}m"

instance HasField "reset" Modifier String where
  getField m = "\ESC[{modifierUnset m}m"

instance ToString a => ToString (Ansi a) where
  toString (Ansi modifiers a) = set <> toString a <> unset
    where
      set :: String
      set
        | modifiers.empty? = ""
        | otherwise = "\ESC[{ (modifiers.map modifierSet).join ";"}m"

      unset :: String
      unset
        | modifiers.empty? = ""
        | otherwise = "\ESC[{ (modifiers.reverse.map modifierUnset).join ";" }m"

instance ToString a => HasField "toString" (Ansi a) String where
  getField = toString

instance HasField "ansi" FilePath (Ansi FilePath) where
  getField = ansi

bold :: Ansi a -> Ansi a
bold = modifier Bold

faint :: Ansi a -> Ansi a
faint = modifier Faint

underline :: Ansi a -> Ansi a
underline = modifier Underline

inverse :: Ansi a -> Ansi a
inverse = modifier Inverse

instance HasField "bold" (Ansi a) (Ansi a) where
  getField = bold

instance HasField "faint" (Ansi a) (Ansi a) where
  getField = faint

instance HasField "underline" (Ansi a) (Ansi a) where
  getField = underline

instance HasField "inverse" (Ansi a) (Ansi a) where
  getField = inverse

black :: Ansi a -> Ansi a
black = foreground Black

red :: Ansi a -> Ansi a
red = foreground Red

green :: Ansi a -> Ansi a
green = foreground Green

yellow :: Ansi a -> Ansi a
yellow = foreground Yellow

blue :: Ansi a -> Ansi a
blue = foreground Blue

magenta :: Ansi a -> Ansi a
magenta = foreground Magenta

cyan :: Ansi a -> Ansi a
cyan = foreground Cyan

white :: Ansi a -> Ansi a
white = foreground White

rgb :: Word8 -> Word8 -> Word8 -> Ansi a -> Ansi a
rgb r g b = foreground (RGB r g b)

on_black :: Ansi a -> Ansi a
on_black = background Black

on_red :: Ansi a -> Ansi a
on_red = background Red

on_green :: Ansi a -> Ansi a
on_green = background Green

on_yellow :: Ansi a -> Ansi a
on_yellow = background Yellow

on_blue :: Ansi a -> Ansi a
on_blue = background Blue

on_magenta :: Ansi a -> Ansi a
on_magenta = background Magenta

on_cyan :: Ansi a -> Ansi a
on_cyan = background Cyan

on_white :: Ansi a -> Ansi a
on_white = background White

on_rgb :: Word8 -> Word8 -> Word8 -> Ansi a -> Ansi a
on_rgb r g b = background (RGB r g b)

instance HasField "black" (Ansi a) (Ansi a) where
  getField = black

instance HasField "red" (Ansi a) (Ansi a) where
  getField = red

instance HasField "green" (Ansi a) (Ansi a) where
  getField = green

instance HasField "yellow" (Ansi a) (Ansi a) where
  getField = yellow

instance HasField "blue" (Ansi a) (Ansi a) where
  getField = blue

instance HasField "magenta" (Ansi a) (Ansi a) where
  getField = magenta

instance HasField "cyan" (Ansi a) (Ansi a) where
  getField = cyan

instance HasField "white" (Ansi a) (Ansi a) where
  getField = white

instance HasField "rgb" (Ansi a) (Word8 -> Word8 -> Word8 -> Ansi a) where
  getField xs r g b = rgb r g b xs

instance HasField "on_black" (Ansi a) (Ansi a) where
  getField = on_black

instance HasField "on_red" (Ansi a) (Ansi a) where
  getField = on_red

instance HasField "on_green" (Ansi a) (Ansi a) where
  getField = on_green

instance HasField "on_yellow" (Ansi a) (Ansi a) where
  getField = on_yellow

instance HasField "on_blue" (Ansi a) (Ansi a) where
  getField = on_blue

instance HasField "on_magenta" (Ansi a) (Ansi a) where
  getField = on_magenta

instance HasField "on_cyan" (Ansi a) (Ansi a) where
  getField = on_cyan

instance HasField "on_white" (Ansi a) (Ansi a) where
  getField = on_white

instance HasField "on_rgb" (Ansi a) (Word8 -> Word8 -> Word8 -> Ansi a) where
  getField xs r g b = on_rgb r g b xs
