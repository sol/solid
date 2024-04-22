{-# LANGUAGE DerivingStrategies #-}
module Solid.PP.Builder (
  Builder
, fromText
, toText

, show
, int
, char
, fastString

, join
, unlines
, concatMap
) where

import           Prelude ()
import           Solid.PP.IO as Prelude hiding (show, join, unlines, concatMap)
import qualified Solid.PP.IO as Prelude

import           Data.List (intersperse)

import           Data.Text.Internal (text)
import           Data.Text.Internal.StrictBuilder (StrictBuilder)
import qualified Data.Text.Internal.StrictBuilder as T
import           Data.ByteString.Short (ShortByteString(..))
import qualified Data.ByteString.Short as ShortByteString

import           Solid.PP.Lexer (FastString, fastStringToShortByteString)

newtype Builder = Builder { unBuilder :: StrictBuilder }
  deriving newtype (Semigroup, Monoid)

instance IsString Builder where
  fromString = fromText . pack

fromText :: Text -> Builder
fromText = Builder . T.fromText

toText :: Builder -> Text
toText = T.toText . unBuilder

show :: Show a => a -> Builder
show = fromString . Prelude.show

int :: Int -> Builder
int = show

char :: Char -> Builder
char = Builder . T.fromChar

fastString :: FastString -> Builder
fastString = unsafeFromShortByteString . fastStringToShortByteString

unsafeFromShortByteString :: ShortByteString -> Builder
unsafeFromShortByteString bs = fromText $ text bs.unShortByteString 0 (ShortByteString.length bs)

join :: Builder -> [Builder] -> Builder
join separator = mconcat . intersperse separator

unlines :: [Builder] -> Builder
unlines = mconcat . foldr (\ x xs -> x : char '\n' : xs) []

concatMap :: (a -> Builder) -> [a] -> Builder
concatMap p = mconcat . map p
