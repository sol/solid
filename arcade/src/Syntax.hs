{-# LANGUAGE OverloadedStrings #-}
module Syntax where

use Haskell
use Solid.PP.Lexer
use Solid.PP

data Foo = Include

tokenize :: String -> Either [Char] Lexer.LexerResult
tokenize = Lexer.tokenize PP.extensions "main.hs" 1 . Haskell.toText
