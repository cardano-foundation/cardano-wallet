module Cardano.Wallet.Primitive.Types.Address.Gen
    ( -- * Generators and shrinkers
      genAddress
    , shrinkAddress

      -- * Indicator functions on addresses
    , addressParity
    , Parity (..)
    )
where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Test.QuickCheck
    ( Gen
    , elements
    , sized
    )

import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

--------------------------------------------------------------------------------
-- Addresses generated according to the size parameter
--------------------------------------------------------------------------------

genAddress :: Gen (Address)
genAddress = sized $ \size -> elements $ take (max 1 size) addresses

shrinkAddress :: Address -> [Address]
shrinkAddress a
    | a == simplest = []
    | otherwise = [simplest]
  where
    simplest = head addresses

addresses :: [Address]
addresses = mkAddress <$> ['0' ..]

--------------------------------------------------------------------------------
-- Indicator functions on addresses
--------------------------------------------------------------------------------

-- | Computes the parity of an address.
--
-- Parity is defined in the following way:
--
--    - even-parity address:
--      an address with a pop count (Hamming weight) that is even.
--
--    - odd-parity address:
--      an address with a pop count (Hamming weight) that is odd.
--
-- Examples of even-parity and odd-parity addresses:
--
--    - 0b00000000 : even (Hamming weight = 0)
--    - 0b00000001 : odd  (Hamming weight = 1)
--    - 0b00000010 : odd  (Hamming weight = 1)
--    - 0b00000011 : even (Hamming weight = 2)
--    - 0b00000100 : odd  (Hamming weight = 1)
--    - ...
--    - 0b11111110 : odd  (Hamming weight = 7)
--    - 0b11111111 : even (Hamming weight = 8)
addressParity :: Address -> Parity
addressParity = parity . addressPopCount
  where
    addressPopCount :: Address -> Int
    addressPopCount = BS.foldl' (\acc -> (acc +) . Bits.popCount) 0 . unAddress

    parity :: Integral a => a -> Parity
    parity a
        | even a = Even
        | otherwise = Odd

-- | Represents the parity of a value (whether the value is even or odd).
data Parity = Even | Odd
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Internal utilities
--------------------------------------------------------------------------------

mkAddress :: Char -> Address
mkAddress c = Address $ "ADDR" `B8.snoc` c
