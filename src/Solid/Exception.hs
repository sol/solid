module Solid.Exception (
  HasCallStack
, Exception
, UnicodeDecodeError(..)
, evaluate
, throw
, throwIO
, bracket
, bracket_
) where

import           Prelude ()
import           Solid.Common

import           GHC.Stack as Imports (HasCallStack)
import           Control.Exception as Imports (Exception, evaluate, throw, throwIO, bracket, bracket_)

data UnicodeDecodeError = UnicodeDecodeError
  deriving (Eq, Show, Exception)
