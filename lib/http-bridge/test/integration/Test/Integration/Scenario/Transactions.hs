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
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": 1,
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]

        r <- request @ApiTransaction ctx ("POST", postTx wa) Default payload
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectFieldEqual amount 168654
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        r' <- request @ApiWallet ctx ("GET", getWallet wb) Default payload
        verify r'
            [ expectSuccess
            , expectEventually ctx balanceAvailable (oneMillionAda + 1)
            ]
  where
    oneMillionAda =
        1 * 1000 * 1000 * 1000 * 1000
    getAddresses (w :: ApiWallet) =
        "v2/wallets/" <> w ^. walletId <> "/addresses"
    postTx (w :: ApiWallet) =
        "v2/wallets/" <> w ^. walletId <> "/transactions"
    getWallet (w :: ApiWallet) =
        "v2/wallets/" <> w ^. walletId
