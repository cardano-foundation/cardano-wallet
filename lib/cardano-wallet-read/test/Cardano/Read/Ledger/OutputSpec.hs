{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Read.Ledger.OutputSpec
    ( spec
    ) where

import Prelude

import Cardano.Ledger.Address
    ( Addr (Addr)
    )
import Cardano.Ledger.Api
    ( mkBasicTxOut
    )
import Cardano.Ledger.BaseTypes
    ( CertIx (CertIx)
    , Network (Mainnet)
    , TxIx (TxIx)
    )
import Cardano.Ledger.Coin
    ( Coin (Coin)
    )
import Cardano.Ledger.Credential
    ( Credential (KeyHashObj)
    , PaymentCredential
    , Ptr (Ptr)
    , SlotNo32 (..)
    , StakeReference (StakeRefPtr)
    )
import Cardano.Ledger.Keys
    ( KeyHash (KeyHash)
    )
import Cardano.Ledger.Val
    ( inject
    )
import Cardano.Read.Ledger.Tx.Output
    ( Output (..)
    , upgradeToOutputConway
    )
import Cardano.Wallet.Read.Eras
    ( Babbage
    )
import Cardano.Wallet.Read.Hash
    ( hashFromBytesAsHex
    )
import Data.ByteString
    ( ByteString
    )
import Data.Maybe
    ( fromMaybe
    )
import Test.Hspec
    ( Spec
    , it
    )
import Test.QuickCheck
    ( (.&&.)
    , (=/=)
    )

spec :: Spec
spec =
    -- Test that TxOut upgrade preserves pointer address differences
    -- (Previously, some invalid pointers would be normalized/equalized)
    it "Conway preserves distinct pointer addresses" $
        (outputPtr1 =/= outputPtr2)
        .&&.
        (upgradeToOutputConway outputPtr1
            =/= upgradeToOutputConway outputPtr2
        )

{-----------------------------------------------------------------------------
    Pointer addresses
------------------------------------------------------------------------------}
outputPtr1 :: Output Babbage
outputPtr1 = Output $ mkBasicTxOut addrInvalidPtr $ inject (Coin 13)

outputPtr2 :: Output Babbage
outputPtr2 = Output $ mkBasicTxOut addrNullPtr $ inject (Coin 13)

-- | Pointer address with large CertIx - should become invalid in Conway
addrInvalidPtr :: Addr
addrInvalidPtr =
    Addr
        Mainnet
        paymentCred
        (StakeRefPtr invalidPtr)
  where
    -- CertIx > 2^16 can't be represented in Conway's compact format
    -- so it gets normalized. Using maxBound to test this edge case.
    invalidPtr = Ptr (SlotNo32 maxBound) (TxIx maxBound) (CertIx maxBound)

-- | Pointer address that's clearly different from addrInvalidPtr
addrNullPtr :: Addr
addrNullPtr =
    Addr
        Mainnet
        paymentCred
        (StakeRefPtr nullPtr)
  where
    nullPtr = Ptr (SlotNo32 0) (TxIx 0) (CertIx 0)

paymentCred :: PaymentCredential
paymentCred =
    mkPaymentCred
        "6505f8fa4e47723170d3e60bbc30d6ec406f368b1c84e1f75b0a8cba"

mkPaymentCred :: ByteString -> PaymentCredential
mkPaymentCred =
    KeyHashObj
    . KeyHash
    . fromMaybe (error "paymentCred: invalid hex length")
    . hashFromBytesAsHex
