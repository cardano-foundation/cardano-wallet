{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiAddress, ApiTransaction, ApiWallet )
import Cardano.Wallet.Primitive.Types
    ( Direction (..), TxStatus (..) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Test.Hspec
    ( SpecWith, it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , amount
    , balanceAvailable
    , balanceTotal
    , direction
    , expectEventually
    , expectFieldEqual
    , expectResponseCode
    , expectSuccess
    , fixtureWallet
    , json
    , oneMillionAda
    , request
    , status
    , unsafeRequest
    , verify
    , walletId
    )

import qualified Network.HTTP.Types.Status as HTTP

spec :: SpecWith Context
spec = do
    it "Check fixture transaction" $ \ctx -> do
        wid <- view walletId <$> fixtureWallet ctx
        r' <- request @ApiWallet ctx ("GET", "v2/wallets/" <> wid) Default Empty
        verify r'
            [ expectFieldEqual balanceTotal oneMillionAda
            , expectFieldEqual balanceAvailable oneMillionAda
            ]

    it "Single Output Transaction" $ \ctx -> do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
        (_, addrs) <-
            unsafeRequest @[ApiAddress] ctx ("GET", getAddresses wb) Empty
        let amt = 1
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        let fee = 168653

        r <- request @ApiTransaction ctx ("POST", postTx wa) Default payload
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectFieldEqual amount (fee + amt)
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        ra <- request @ApiWallet ctx ("GET", getWallet wa) Default payload
        verify ra
            [ expectSuccess
            , expectFieldEqual balanceTotal (oneMillionAda - fee - amt)
            , expectFieldEqual balanceAvailable 0
            ]

        rb <- request @ApiWallet ctx ("GET", getWallet wb) Default payload
        verify rb
            [ expectSuccess
            , expectEventually ctx balanceAvailable (oneMillionAda + amt)
            ]

        verify ra
            [ expectEventually ctx balanceAvailable (oneMillionAda - fee - amt)
            ]
  where
    getAddresses (w :: ApiWallet) =
        "v2/wallets/" <> w ^. walletId <> "/addresses"
    postTx (w :: ApiWallet) =
        "v2/wallets/" <> w ^. walletId <> "/transactions"
    getWallet (w :: ApiWallet) =
        "v2/wallets/" <> w ^. walletId
