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

import           GHC.Types.SrcLoc

type WithBufferSpan = GenLocated BufferSpan

newtype StartColumn = StartColumn Int
  deriving newtype (Eq, Show, Num)

data BufferSpan = BufferSpan {
  start :: Int
, end :: Int
, startColumn :: StartColumn
} deriving (Eq, Show)

instance HasField "length" BufferSpan Int where
  getField loc = loc.end - loc.start

toBufferSpan :: PsSpan -> BufferSpan
toBufferSpan PsSpan{..} = BufferSpan {..}
  where
    start = bufPos $ bufSpanStart psBufSpan
    end = bufPos $ bufSpanEnd psBufSpan
    startColumn = StartColumn . srcLocCol $ realSrcSpanStart psRealSpan
