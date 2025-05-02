module Solid.PP.Lexer.Fast where

import Data.Word
import Data.Bits

-- |
-- O(1) check whether a 'Word8' is one of:
-- [33,35,36,37,38,42,43,45,46,47,58,60,61,62,63,64,92,94,124,126]
isSymbol :: Word8 -> Bool
isSymbol !c = (bitset `unsafeShiftR` i .&. 1) /= 0
  where
    !i = fromIntegral (c .&. 0b111111) :: Int

    !bitset = case c `unsafeShiftR` 6 of
      0 -> 0xf400ec7a00000000 :: Word64 -- [33,35,36,37,38,42,43,45,46,47,58,60,61,62,63]
      1 -> 0x5000000050000001 :: Word64 -- [64,92,94,124,126]
      _ -> 0
