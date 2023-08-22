{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
module Solid.PP.SrcLoc (
  SrcLoc(..)
, fromPsLoc

, WithBufferSpan
, GenLocated(..)
, unLoc
, getLoc

, StartColumn(..)
, BufferSpan(..)
, toBufferSpan
) where

import           Prelude ()
import           Solid.PP.IO

import           Data.Coerce (coerce)
import           GHC.Data.FastString (unpackFS)
import           GHC.Types.SrcLoc hiding (SrcLoc, srcSpanStart, srcSpanEnd)

data SrcLoc = SrcLoc {
  file :: ~FilePath
, offset :: ~Int
, line :: ~Int
, column :: ~Int
} deriving (Eq, Show, Ord)

fromPsLoc :: PsLoc -> SrcLoc
fromPsLoc loc = SrcLoc {
  file = unpackFS (srcLocFile loc.psRealLoc)
, offset = loc.psBufPos.bufPos
, line = srcLocLine loc.psRealLoc
, column = srcLocCol loc.psRealLoc
}

type WithBufferSpan = GenLocated BufferSpan

newtype StartColumn = StartColumn Int
  deriving newtype (Eq, Show, Num, Ord)

data BufferSpan = BufferSpan {
  file :: ~FilePath
, start :: Int
, end :: Int
, startLine :: Int
, endLine :: Int
, startColumn :: StartColumn
, endColumn :: Int
} deriving (Eq, Show, Ord)

instance HasField "length" BufferSpan Int where
  getField loc = loc.end - loc.start

instance HasField "startLoc" BufferSpan SrcLoc where
  getField loc = SrcLoc {
      file = loc.file
    , offset = loc.start
    , line = loc.startLine
    , column = coerce loc.startColumn
    }

instance HasField "endLoc" BufferSpan SrcLoc where
  getField loc = SrcLoc {
      file = loc.file
    , offset = loc.end
    , line = loc.endLine
    , column = loc.endColumn
    }

instance HasField "merge" BufferSpan (BufferSpan -> BufferSpan) where
  getField start end = start {
      end = end.end
    , endLine = end.endLine
    , endColumn = end.endColumn
    }

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
