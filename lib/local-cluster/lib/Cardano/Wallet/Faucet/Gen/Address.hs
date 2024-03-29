{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Faucet.Gen.Address
    ( genAddress
    )
where

import Prelude

import Cardano.Address
    ( Address
    , unsafeMkAddress
    )
import Data.Bits
    ( Bits (shiftL, (.|.))
    )
import Data.ByteString
    ( ByteString
    )
import Data.Word
    ( Word8
    )
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , elements
    , vectorOf
    )

import qualified Data.ByteString as BS

{-
ADDRESS = %b0000 | NETWORK-TAG | KEY-HASH    | KEY-HASH       ; type 00, Base Shelley address
        \ %b0001 | NETWORK-TAG | SCRIPT-HASH | KEY-HASH       ; type 01, Base Shelley address
        \ %b0010 | NETWORK-TAG | KEY-HASH    | SCRIPT-HASH    ; type 02, Base Shelley address
        \ %b0011 | NETWORK-TAG | SCRIPT-HASH | SCRIPT-HASH    ; type 03, Base Shelley address
        \ %b0100 | NETWORK-TAG | KEY-HASH    | POINTER        ; type 04, Pointer Shelley address
        \ %b0101 | NETWORK-TAG | SCRIPT-HASH | POINTER        ; type 05, Pointer Shelley address
        \ %b0110 | NETWORK-TAG | KEY-HASH                     ; type 06, Payment Shelley address
        \ %b0111 | NETWORK-TAG | SCRIPT-HASH                  ; type 07, Payment Shelley address
        \ %b1000 | BYRON-PAYLOAD                              ; type 08, Byron / Bootstrap address
        \ %b1110 | NETWORK-TAG | KEY-HASH                     ; type 14, Stake Shelley address
        \ %b1111 | NETWORK-TAG | SCRIPT-HASH                  ; type 15, Stake Shelley address

NETWORK-TAG  = %b0000 ; Testnet
             \ %b0001 ; Mainnet

POINTER = VARIABLE-LENGTH-UINT ; slot number
        | VARIABLE-LENGTH-UINT ; transaction index
        | VARIABLE-LENGTH-UINT ; certificate index

VARIABLE-LENGTH-UINT = (%b1 | UINT7 | VARIABLE-LENGTH-UINT)
                     / (%b0 | UINT7)
UINT7 = 7BIT

KEY-HASH = 28OCTET

SCRIPT-HASH= 28OCTET

BYRON-PAYLOAD = *OCTET ; see 'Byron Addresses' section or cddl specification.

-}

genPrefix :: [Word8] -> Gen Word8
genPrefix xs = do
    network <- elements [0, 1]
    prefix <- elements xs
    pure $ prefix `shiftL` 4 .|. network

genHash :: Gen ByteString
genHash = BS.pack <$> vectorOf 28 arbitrary

-- | Generate a random address excluding the Byron addresses.
genAddress :: Gen Address
genAddress = fmap unsafeMkAddress $ do
    hash1 <- genHash
    hash2 <- genHash
    prefixONE <- genPrefix [6, 7, 14, 15]
    prefixTWO <- genPrefix [0, 1, 2, 3]
    elements
        [ prefixONE `BS.cons` hash1
        , prefixTWO `BS.cons` hash1 <> hash2
        ]
