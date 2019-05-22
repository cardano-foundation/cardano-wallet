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
    , expectFieldBetween
    , expectFieldEqual
    , expectResponseCode
    , expectSuccess
    , faucetAmt
    , faucetUtxoAmt
    , fixtureWallet
    , json
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
            [ expectFieldEqual balanceTotal faucetAmt
            , expectFieldEqual balanceAvailable faucetAmt
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
        let (feeMin, feeMax) = (168653, 168829)

        r <- request @ApiTransaction ctx ("POST", postTx wa) Default payload
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectFieldBetween amount (feeMin + amt, feeMax + amt)
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        ra <- request @ApiWallet ctx ("GET", getWallet wa) Default Empty
        verify ra
            [ expectSuccess
            , expectFieldBetween balanceTotal
                ( faucetAmt - feeMax - amt
                , faucetAmt - feeMin - amt
                )
            , expectFieldEqual balanceAvailable (faucetAmt - faucetUtxoAmt)
            ]

        rb <- request @ApiWallet ctx ("GET", getWallet wb) Default Empty
        verify rb
            [ expectSuccess
            , expectEventually ctx balanceAvailable (faucetAmt + amt)
            ]

        verify ra
            [ expectEventually ctx balanceAvailable (faucetAmt - feeMax - amt)
            ]

    it "TRANS_CREATE_02 - Multiple Output Transaction to single wallet" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- createWallet ctx "Test dest1" mnemonics18
        (_, addrs1) <-
            unsafeRequest @[ApiAddress] ctx ("GET", getAddresses wDest) Empty

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

        r <- request @ApiTransaction ctx ("POST", postTx wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectFieldBetween amount (feeMin + amt, feeMax + amt)
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        ra <- request @ApiWallet ctx ("GET", getWallet wSrc) Default Empty
        verify ra
            [ expectFieldBetween balanceTotal
                ( faucetAmt - feeMax - amt
                , faucetAmt - feeMin - amt
                )
            , expectFieldEqual balanceAvailable (faucetAmt - 2 * faucetUtxoAmt)
            ]

        rd <- request @ApiWallet ctx ("GET", getWallet wDest) Default payload
        verify rd
            [ expectEventually ctx balanceAvailable (2*amt)
            , expectEventually ctx balanceTotal (2*amt)
            ]

    it "TRANS_CREATE_02 - Multiple Output Transaction to different wallets"
        $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest1 <- createWallet ctx "Test dest1" mnemonics18
        wDest2 <- createWallet ctx "Test dest2" mnemonics21
        (_, addrs1) <-
            unsafeRequest @[ApiAddress] ctx ("GET", getAddresses wDest1) Empty
        (_, addrs2) <-
            unsafeRequest @[ApiAddress] ctx ("GET", getAddresses wDest2) Empty

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

        r <- request @ApiTransaction ctx ("POST", postTx wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectFieldBetween amount (feeMin + amt, feeMax + amt)
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        ra <- request @ApiWallet ctx ("GET", getWallet wSrc) Default Empty
        verify ra
            [ expectFieldBetween balanceTotal
                ( faucetAmt - feeMax - amt
                , faucetAmt - feeMin - amt
                )
            , expectFieldEqual balanceAvailable (faucetAmt - 2 * faucetUtxoAmt)
            ]

        forM_ [wDest1, wDest2] $ \wDest -> do
            rd <- request @ApiWallet ctx ("GET", getWallet wDest) Default payload
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
