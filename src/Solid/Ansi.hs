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
) where

import Solid.Common
import Solid.ToString
import List ()
import String (String)
import FilePath (FilePath)

import Solid.Ansi.Types

modifier :: Modifier -> Ansi a -> Ansi a
modifier m (Ansi ms a) = Ansi (m : ms) a

foreground :: Color -> Ansi a -> Ansi a
foreground = modifier . Foreground

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

modifierUnset :: Modifier -> String
modifierUnset = \ case
  Bold -> "22"
  Faint -> "22"
  Underline -> "24"
  Inverse -> "27"
  Foreground _ -> "39"

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
