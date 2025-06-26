{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
module Solid.PP.NewLexer.Type where

import Data.ByteString.Internal (w2c)
import Data.Text.Array (Array)
import Data.Text.Array qualified as A
import Data.Array.Byte (ByteArray)
import GHC.Records
import Data.Text.Internal (Text(..))
import Data.Text ()

new :: Text -> Lexer
new (Text arr start remaining) = Lexer arr start remaining

data Lexer = Lexer {
  arr :: {-# UNPACK #-} !ByteArray
, offset :: {-# UNPACK #-} !Int
, remaining :: {-# UNPACK #-} !Int
} deriving (Show, Eq)

instance HasField "input" Lexer Text where -- FIXME: remove
  getField Lexer{..} = Text arr offset remaining
  {-# INLINE getField #-}

advance :: Int -> Lexer -> Lexer
advance n lexer = unsafeAdvance (min n lexer.remaining) lexer
{-# INLINE advance #-}

unsafeAdvance :: Int -> Lexer -> Lexer
unsafeAdvance n lexer@Lexer{..} = lexer { offset = offset + n, remaining = remaining - n }
{-# INLINE unsafeAdvance #-}

advanceWhile :: (Char -> Bool) -> Lexer -> Lexer
advanceWhile p lexer@Lexer{..} = unsafeAdvance (go 0) lexer
  where
    go :: Int -> Int
    go n
      | n < remaining = if p (unsafeIndex arr (offset + n)) then go (n + 1) else n
      | otherwise = n
{-# INLINE advanceWhile #-}

index :: Int -> Lexer -> Char
index n Lexer{..}
  | n < remaining = unsafeIndex arr (offset + n)
  | otherwise = '\0'
{-# INLINE index #-}

next :: Lexer -> (Char, Lexer)
next lexer@Lexer{..}
  | 0 < remaining = (unsafeIndex arr offset, unsafeAdvance 1 lexer)
  | otherwise = ('\0', lexer)
{-# INLINE next #-}

unsafeIndex :: Array -> Int -> Char
unsafeIndex arr = w2c . A.unsafeIndex arr
{-# INLINE unsafeIndex #-}
