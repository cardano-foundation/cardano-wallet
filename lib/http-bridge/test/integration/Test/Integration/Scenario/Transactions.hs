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
    , createWallet
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
import Test.Integration.Framework.TestData
    ( mnemonics18, mnemonics21 )

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

    it "TRANS_CREATE_01 - Single Output Transaction" $ \ctx -> do
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

        ra <- request @ApiWallet ctx ("GET", getWallet wa) Default Empty
        verify ra
            [ expectSuccess
            , expectFieldEqual balanceTotal (oneMillionAda - fee - amt)
            , expectFieldEqual balanceAvailable 0
            ]

        rb <- request @ApiWallet ctx ("GET", getWallet wb) Default Empty
        verify rb
            [ expectSuccess
            , expectEventually ctx balanceAvailable (oneMillionAda + amt)
            ]

        verify ra
            [ expectEventually ctx balanceAvailable (oneMillionAda - fee - amt)
            ]

    it "TRANS_CREATE_02 - Multiple Output Transaction to single wallet" $ \ctx -> do
        widSrc <- view walletId <$> fixtureWallet ctx
        widDest <- createWallet ctx "Test dest1" mnemonics18
        (_, addrs1) <-
            unsafeRequest @[ApiAddress] ctx
                ("GET", "v2/wallets/" <> widDest <> "/addresses") Empty

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
        let fee = 168653

        --post transaction
        r <- request @ApiTransaction ctx
            ("POST", "v2/wallets/" <> widSrc <> "/transactions")
            Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectFieldEqual amount (fee + amt + amt)
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        --check immediate balanceTotal and balanceAvailable on source wallet
        ra <- request @ApiWallet ctx ("GET", "v2/wallets/" <> widSrc)
            Default Empty
        verify ra
            [ expectFieldEqual balanceTotal (oneMillionAda - fee - amt - amt)
            , expectFieldEqual balanceAvailable 0
            ]

        --check balanceTotal and balanceAvailable on destination wallet
        rd <- request @ApiWallet ctx ("GET", "v2/wallets/" <> widDest)
            Default payload
        verify rd
            [ expectEventually ctx balanceAvailable (2*amt)
            , expectEventually ctx balanceTotal (2*amt)
            ]

    it "TRANS_CREATE_02 - Multiple Output Transaction to different wallets"
        $ \ctx -> do
        wa <- fixtureWallet ctx
        widDest1 <- createWallet ctx "Test dest1" mnemonics18
        widDest2 <- createWallet ctx "Test dest2" mnemonics21
        (_, addrs1) <-
            unsafeRequest @[ApiAddress] ctx
                ("GET", "v2/wallets/" <> widDest1 <> "/addresses") Empty
        (_, addrs2) <-
            unsafeRequest @[ApiAddress] ctx
                ("GET", "v2/wallets/" <> widDest2 <> "/addresses") Empty

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
        let fee = 168653

        --post transaction
        r <- request @ApiTransaction ctx ("POST", postTx wa) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectFieldEqual amount (fee + amt + amt)
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        --check immediate balanceTotal and balanceAvailable on source wallet
        ra <- request @ApiWallet ctx ("GET", getWallet wa) Default Empty
        verify ra
            [ expectFieldEqual balanceTotal (oneMillionAda - fee - amt - amt)
            , expectFieldEqual balanceAvailable 0
            ]

        --check balanceTotal and balanceAvailable on destination wallets
        forM_ [widDest1, widDest2] $ \walId -> do
            rd <- request @ApiWallet ctx ("GET", "v2/wallets/" <> walId)
                Default payload
            verify rd
                [ expectSuccess
                , expectEventually ctx balanceAvailable amt
                , expectEventually ctx balanceTotal amt
                ]

--    it "TRANS_CREATE_02 - Multiple Output Transactions don't work on single UTxO" $ \ctx -> do
--        widSrc <- view walletId <$> fixtureWallet ctx
--        Stdout mnemonics <- generateMnemonicsViaCLI []
--        widDest <- createWallet ctx "Test dest1" (T.words $ T.pack mnemonics)
--        (_, addrs) <-
--            unsafeRequest @[ApiAddress] ctx
--                ("GET", "v2/wallets/" <> widDest <> "/addresses") Empty
--
--        let destination1 = (addrs !! 1) ^. #id
--        let destination2 = (addrs !! 2) ^. #id
--        let payload = Json [json|{
--                  "payments": [
--                                       {
--                                               "address": #{destination1},
--                                               "amount": {
--                                                       "quantity": 1,
--                                                       "unit": "lovelace"
--                                               }
--                                       },
--                                       {
--                                               "address": #{destination2},
--                                               "amount": {
--                                                       "quantity": 1,
--                                                       "unit": "lovelace"
--                                               }
--                                       }
--                               ],
--                   "passphrase": "Secure Passphrase"
--                }|]
--
--        --post transaction
--        r <- request @ApiTransaction ctx
--            ("POST", "v2/wallets/" <> widSrc <> "/transactions") Default payload
--        verify r
--            [ expectResponseCode HTTP.status403
--            , expectErrorMessage "There's a restriction in the way I can construct\
--                \ transactions: I do not re-use a same UTxO for different outputs.\
--                \ Here, I only have 1 available but there are 2 outputs."
--            ]
  where
    getAddresses (w :: ApiWallet) =
        "v2/wallets/" <> w ^. walletId <> "/addresses"
    postTx (w :: ApiWallet) =
        "v2/wallets/" <> w ^. walletId <> "/transactions"
    getWallet (w :: ApiWallet) =
        "v2/wallets/" <> w ^. walletId
