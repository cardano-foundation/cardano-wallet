{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
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
import Control.Monad
    ( forM_ )
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
    , emptyWallet
    , expectErrorMessage
    , expectEventually
    , expectFieldBetween
    , expectFieldEqual
    , expectResponseCode
    , expectSuccess
    , faucetAmt
    , faucetUtxoAmt
    , fixtureWallet
    , fixtureWalletWith
    , getAddresses
    , getWallet
    , json
    , postTx
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
            [ expectFieldEqual balanceTotal faucetAmt
            , expectFieldEqual balanceAvailable faucetAmt
            ]

    it "TRANS_CREATE_01 - Single Output Transaction" $ \ctx -> do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
        (_, addrs) <-
            unsafeRequest @[ApiAddress] ctx (getAddresses wb) Empty
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
        let (feeMin, feeMax) = (168653, 168829)

        r <- request @ApiTransaction ctx (postTx wa) Default payload
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectFieldBetween amount (feeMin + amt, feeMax + amt)
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        ra <- request @ApiWallet ctx (getWallet wa) Default Empty
        verify ra
            [ expectSuccess
            , expectFieldBetween balanceTotal
                ( faucetAmt - feeMax - amt
                , faucetAmt - feeMin - amt
                )
            , expectFieldEqual balanceAvailable (faucetAmt - faucetUtxoAmt)
            ]

        rb <- request @ApiWallet ctx (getWallet wb) Default Empty
        verify rb
            [ expectSuccess
            , expectEventually ctx balanceAvailable (faucetAmt + amt)
            ]

        verify ra
            [ expectEventually ctx balanceAvailable (faucetAmt - feeMax - amt)
            ]

    it "TRANS_CREATE_02 - Multiple Output Tx to single wallet" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        (_, addrs1) <-
            unsafeRequest @[ApiAddress] ctx (getAddresses wDest) Empty

        let amt = 1
        let destination1 = (addrs1 !! 1) ^. #id
        let destination2 = (addrs1 !! 2) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination1},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                },
                {
                    "address": #{destination2},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        let (feeMin, feeMax) = (181488, 181840)

        r <- request @ApiTransaction ctx (postTx wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectFieldBetween amount (feeMin + amt, feeMax + amt)
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        ra <- request @ApiWallet ctx (getWallet wSrc) Default Empty
        verify ra
            [ expectFieldBetween balanceTotal
                ( faucetAmt - feeMax - amt
                , faucetAmt - feeMin - amt
                )
            , expectFieldEqual balanceAvailable (faucetAmt - 2 * faucetUtxoAmt)
            ]

        rd <- request @ApiWallet ctx (getWallet wDest) Default Empty
        verify rd
            [ expectEventually ctx balanceAvailable (2*amt)
            , expectEventually ctx balanceTotal (2*amt)
            ]

    it "TRANS_CREATE_02 - Multiple Output Tx to different wallets" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest1 <- emptyWallet ctx
        wDest2 <- emptyWallet ctx
        (_, addrs1) <-
            unsafeRequest @[ApiAddress] ctx (getAddresses wDest1) Empty
        (_, addrs2) <-
            unsafeRequest @[ApiAddress] ctx (getAddresses wDest2) Empty

        let amt = 1
        let destination1 = (addrs1 !! 1) ^. #id
        let destination2 = (addrs2 !! 1) ^. #id
        let payload = Json [json|{
                "payments": [
                    {
                        "address": #{destination1},
                        "amount": {
                            "quantity": #{amt},
                            "unit": "lovelace"
                        }
                    },
                    {
                        "address": #{destination2},
                        "amount": {
                            "quantity": #{amt},
                            "unit": "lovelace"
                        }
                    }
                ],
                "passphrase": "cardano-wallet"
            }|]
        let (feeMin, feeMax) = (181488, 181840)

        r <- request @ApiTransaction ctx (postTx wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectFieldBetween amount (feeMin + amt, feeMax + amt)
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        ra <- request @ApiWallet ctx (getWallet wSrc) Default Empty
        verify ra
            [ expectFieldBetween balanceTotal
                ( faucetAmt - feeMax - amt
                , faucetAmt - feeMin - amt
                )
            , expectFieldEqual balanceAvailable (faucetAmt - 2 * faucetUtxoAmt)
            ]

        forM_ [wDest1, wDest2] $ \wDest -> do
            rd <- request @ApiWallet ctx (getWallet wDest) Default payload
            verify rd
                [ expectSuccess
                , expectEventually ctx balanceAvailable amt
                , expectEventually ctx balanceTotal amt
                ]

    it "TRANS_CREATE_02 - Multiple Output Transactions don't work on single UTxO" $ \ctx -> do
        wUtxo <- fixtureWalletWith ctx [200_000]
        wDest <- emptyWallet ctx
        (_, addrs) <-
            unsafeRequest @[ApiAddress] ctx (getAddresses wDest) Empty

        let destination1 = (addrs !! 1) ^. #id
        let destination2 = (addrs !! 2) ^. #id
        let payload = Json [json|{
                "payments": [
                    {
                        "address": #{destination1},
                        "amount": {
                            "quantity": 1,
                            "unit": "lovelace"
                        }
                    },
                    {
                        "address": #{destination2},
                        "amount": {
                            "quantity": 1,
                            "unit": "lovelace"
                        }
                    }
                ],
                "passphrase": "Secure Passphrase"
            }|]

        r <- request @ApiTransaction ctx (postTx wUtxo) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage
                "When creating new transactions, I'm not able to re-use the \
                \same UTxO for different outputs. Here, I only have 1 \
                \available, but there are 2 outputs."
           ]
