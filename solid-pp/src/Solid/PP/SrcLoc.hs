{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Solid.PP.SrcLoc (
  WithBufferSpan
, GenLocated(..)
, unLoc
, getLoc

, StartColumn(..)
, BufferSpan(..)
, toBufferSpan
) where

import           Prelude ()
import           Solid.PP.IO

import           GHC.Data.FastString (unpackFS)
import           GHC.Types.SrcLoc hiding (srcSpanStart, srcSpanEnd)

type WithBufferSpan = GenLocated BufferSpan

newtype StartColumn = StartColumn Int
  deriving newtype (Eq, Show, Num)

data BufferSpan = BufferSpan {
  file :: ~FilePath
, start :: Int
, end :: Int
, startLine :: Int
, endLine :: Int
, startColumn :: StartColumn
, endColumn :: Int
} deriving (Eq, Show)

instance HasField "length" BufferSpan Int where
  getField loc = loc.end - loc.start

toBufferSpan :: PsSpan -> BufferSpan
toBufferSpan PsSpan{..} = BufferSpan {
  file = unpackFS $ srcLocFile srcSpanStart
, start = bufPos $ bufSpanStart psBufSpan
, end = bufPos $ bufSpanEnd psBufSpan
, startLine = srcLocLine srcSpanStart
, endLine = srcLocLine srcSpanEnd
, startColumn = StartColumn $ srcLocCol srcSpanStart
, endColumn = srcLocCol srcSpanEnd
}
  where
    srcSpanStart = realSrcSpanStart psRealSpan
    srcSpanEnd = realSrcSpanEnd psRealSpan
