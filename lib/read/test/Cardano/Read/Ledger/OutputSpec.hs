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
    ( StandardCrypto
    , mkBasicTxOut
    )
import Cardano.Ledger.BaseTypes
    ( Network (Mainnet)
    )
import Cardano.Ledger.Coin
    ( Coin (Coin)
    )
import Cardano.Ledger.Credential
    ( Credential (KeyHashObj)
    , PaymentCredential
    , Ptr (Ptr)
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
import Data.Word
    ( Word16
    )
import Test.Hspec
    ( Spec
    , it
    )
import Test.QuickCheck
    ( (.&&.)
    , (=/=)
    , (===)
    )

spec :: Spec
spec =
    it "Conway equalizes TxOut with invalid pointer addresses" $
        (outputPtr1 =/= outputPtr2)
        .&&.
        (upgradeToOutputConway outputPtr1
            === upgradeToOutputConway outputPtr2
        )

{-----------------------------------------------------------------------------
    Pointer addresses
------------------------------------------------------------------------------}
outputPtr1 :: Output Babbage
outputPtr1 = Output $ mkBasicTxOut addrInvalidPtr $ inject (Coin 13)

outputPtr2 :: Output Babbage
outputPtr2 = Output $ mkBasicTxOut addrNullPtr $ inject (Coin 13)

addrInvalidPtr :: Addr StandardCrypto
addrInvalidPtr =
    Addr
        Mainnet
        paymentCred
        (StakeRefPtr invalidPtr)
  where
    invalidPtr = Ptr (toEnum 0) (toEnum 0) (toEnum maxWord16plus3)
    maxWord16plus3 :: Int
    maxWord16plus3 = fromIntegral (maxBound :: Word16) + 3

addrNullPtr :: Addr StandardCrypto
addrNullPtr =
    Addr
        Mainnet
        paymentCred
        (StakeRefPtr nullPtr)
  where
    nullPtr = Ptr (toEnum 0) (toEnum 0) (toEnum 0)

paymentCred :: PaymentCredential StandardCrypto
paymentCred =
    mkPaymentCred
        "6505f8fa4e47723170d3e60bbc30d6ec406f368b1c84e1f75b0a8cba"

mkPaymentCred :: ByteString -> PaymentCredential StandardCrypto
mkPaymentCred =
    KeyHashObj
    . KeyHash
    . fromMaybe (error "paymentCred: invalid hex length")
    . hashFromBytesAsHex
