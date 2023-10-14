{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImplicitParams #-}

module Solid.StackTrace (
  WithStackTrace
, StackTrace
, toString

, empty
, retrieve

, suppress
, suppressForMethod

-- * Operations
, empty?
, size
, pop
, callSites

-- * Haskell
, toCallStack
, fromCallStack
) where

import Solid.Common hiding (empty)
import Solid.String
import Solid.ToString qualified as Solid
use Solid.Bytes

import Data.Function (fix)
import Data.List qualified as Haskell
import GHC.Stack qualified as GHC
import GHC.Stack.Types qualified as GHC

deriving instance Eq GHC.CallStack

newtype StackTrace = StackTrace GHC.CallStack
  deriving newtype (Eq, Show)

instance Solid.ToString StackTrace where
  toString = toString

instance HasField "toString" StackTrace String where
  getField = toString

toString :: StackTrace -> String
toString = prettyCallStack . toCallStack

empty :: StackTrace
empty = StackTrace GHC.EmptyCallStack

retrieve :: WithStackTrace => StackTrace
retrieve = (fromCallStack GHC.callStack).pop

suppress :: WithStackTrace => (WithStackTrace => a) -> a
suppress expression = let ?callStack = GHC.freezeCallStack (GHC.popCallStack GHC.callStack) in expression

suppressForMethod :: String -> (WithStackTrace => a) -> a
suppressForMethod name expression = let ?callStack = stack in expression
  where
    stack :: GHC.CallStack
    stack = GHC.freezeCallStack $ GHC.fromCallSiteList [(unpack name, unknownLocation)]

unknownLocation :: GHC.SrcLoc
unknownLocation = GHC.SrcLoc {
  srcLocPackage   = "solid"
, srcLocModule    = "Solid.StackTrace"
, srcLocFile      = "<unknown location>"
, srcLocStartLine = 0
, srcLocStartCol  = 0
, srcLocEndLine   = 0
, srcLocEndCol    = 0
}

empty? :: StackTrace -> Bool
empty? = go . toCallStack
  where
    go = \ case
      GHC.EmptyCallStack -> True
      GHC.PushCallStack _ _ _ -> False
      GHC.FreezeCallStack stack -> go stack

size :: StackTrace -> Int
size = Haskell.length . GHC.getCallStack . toCallStack

pop :: StackTrace -> StackTrace
pop (StackTrace stack) = StackTrace $ case stack of
  GHC.EmptyCallStack -> stack
  GHC.PushCallStack _ _ st -> st
  GHC.FreezeCallStack _ -> stack

callSites :: StackTrace -> [String]
callSites = map (fst >>> pack) . GHC.getCallStack . toCallStack

instance HasField "empty\660" StackTrace Bool where
  getField = empty?

instance HasField "size" StackTrace Int where
  getField = size

instance HasField "pop" StackTrace StackTrace where
  getField = pop

instance HasField "callSites" StackTrace [String] where
  getField = callSites

toCallStack :: StackTrace -> GHC.CallStack
toCallStack (StackTrace stack) = stack

fromCallStack :: GHC.CallStack -> StackTrace
fromCallStack = StackTrace . go
  where
    go :: GHC.CallStack -> GHC.CallStack
    go = \ case
      GHC.EmptyCallStack -> GHC.EmptyCallStack
      GHC.PushCallStack site loc stack -> GHC.PushCallStack (unmangleIdentifier site) loc (go stack)
      GHC.FreezeCallStack stack -> GHC.FreezeCallStack (go stack)

    unmangleIdentifier :: [Char] -> [Char]
    unmangleIdentifier = fix $ \ rec -> \ case
      [] -> []
      '\660' : xs -> '?' : rec xs
      '\7433' : xs -> '!' : rec xs
      x : xs -> x : rec xs

prettyCallStack :: GHC.CallStack -> String
prettyCallStack stack = Bytes.intercalate "\n" $ case GHC.getCallStack stack of
  []  -> []
  stk -> "StackTrace (from WithStackTrace):" : map prettyCallSite stk
  where
    prettyCallSite :: ([Char], GHC.SrcLoc) -> String
    prettyCallSite (name, loc) = "  {pack name}, called at {prettySrcLoc loc}"

    prettySrcLoc :: GHC.SrcLoc -> String
    prettySrcLoc loc
      | loc == unknownLocation = "<unknown location>"
      | otherwise = "{pack loc.srcLocFile}:{loc.srcLocStartLine}:{loc.srcLocStartCol} in {pack loc.srcLocPackage}:{pack loc.srcLocModule}"
