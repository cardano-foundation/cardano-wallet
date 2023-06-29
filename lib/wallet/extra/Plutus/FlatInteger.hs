-- | Encoding of 'Integer' into sequence of bits
-- via the encoding defined in the @flat@ package.
--
-- This encoding is useful for hex editing compiled and serialised
-- Plutus contracts.
module FlatInteger where

import Prelude

import Data.Bits
    ( (.&.)
    , (.|.)
    )
import Data.Word
    ( Word8
    )
import Numeric.Natural
    ( Natural
    )

import qualified Data.Bits as Bits

{-----------------------------------------------------------------------------
    Encoding of Integer
------------------------------------------------------------------------------}

-- | Encode an 'Integer' into a sequence of bits
-- via the encoding defined in the @flat@ package.
flatInteger :: Integer -> [Word8]
flatInteger = word7s . zigZag

-- | Map signed 'Integer' to unsigned 'Natural'
-- such that the absolute value stays small.
zigZag :: Integer -> Natural
zigZag x
    | x >= 0 = fromIntegral $ x `Bits.shiftL` 1
    | otherwise = fromIntegral $ negate (x `Bits.shiftL` 1) - 1

-- | Split into chunks of 7 bits.
--
-- In the result, bit number 8 is used to indicate that
-- the sequence continues.
word7s :: Natural -> [Word8]
word7s x
    | high == 0 = [low7]
    | otherwise = (low7 .|. 0x80) : word7s high
  where
    low7 = (fromIntegral x :: Word8) .&. 0x7F -- lowest 7 bits
    high = x `Bits.shiftR` 7 -- remove 7 lowest bits

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}

-- | Convert a bytes into a sequence of bits.
-- The least significant bit comes last, e.g.
--
-- > toBits 0x03 = [False,False,False,False,False,False,True,True]
toBits :: Word8 -> [Bool]
toBits x = map (Bits.testBit x) [7, 6, 5, 4, 3, 2, 1, 0]

-- | Show the bits corresponding to a sequence of bytes.
--
-- (Least significant bits come last.)
showBits :: [Word8] -> String
showBits = unwords . map show8
  where
    show1 b = if b then "1" else "0"
    show8 = concatMap show1 . toBits
