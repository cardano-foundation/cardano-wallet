{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.API.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiFee, ApiTransaction, ApiWallet )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress (..), Direction (..), EncodeAddress (..), TxStatus (..) )
import Control.Monad
    ( forM_ )
import Data.Aeson
    ( Value )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , TxDescription (..)
    , amount
    , balanceAvailable
    , balanceTotal
    , deleteWalletEp
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
    , feeEstimator
    , fixtureWallet
    , fixtureWalletWith
    , getWalletEp
    , json
    , listAddresses
    , postTxEp
    , postTxFeeEp
    , request
    , status
    , verify
    , walletId
    )
import Test.Integration.Framework.Request
    ( RequestException )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , errMsg403Fee
    , errMsg403InputsDepleted
    , errMsg403NotEnoughMoney
    , errMsg403UTxO
    , errMsg403WrongPass
    , errMsg404NoEndpoint
    , errMsg404NoWallet
    , errMsg405
    , errMsg406
    , errMsg415
    , falseWalletIds
    , kanjiWalletName
    , polishWalletName
    , wildcardsWalletName
    )

import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t. (EncodeAddress t, DecodeAddress t) => SpecWith (Context t)
spec = do

    it "TRANS_CREATE_01 - Single Output Transaction" $ \ctx -> do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
        addrs <- listAddresses ctx wb

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
        let (feeMin, feeMax) = ctx ^. feeEstimator $ TxDescription
                { nInputs = 1
                , nOutputs = 1
                }

        r <- request @(ApiTransaction t) ctx (postTxEp wa) Default payload
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectFieldBetween amount (feeMin + amt, feeMax + amt)
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        ra <- request @ApiWallet ctx (getWalletEp wa) Default Empty
        verify ra
            [ expectSuccess
            , expectFieldBetween balanceTotal
                ( faucetAmt - feeMax - amt
                , faucetAmt - feeMin - amt
                )
            , expectFieldEqual balanceAvailable (faucetAmt - faucetUtxoAmt)
            ]

        rb <- request @ApiWallet ctx (getWalletEp wb) Default Empty
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
        addrs <- listAddresses ctx wDest

        let amt = 1
        let destination1 = (addrs !! 1) ^. #id
        let destination2 = (addrs !! 2) ^. #id
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
        let (feeMin, feeMax) = ctx ^. feeEstimator $ TxDescription
                { nInputs = 2
                , nOutputs = 2
                }

        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        ra <- request @ApiWallet ctx (getWalletEp wSrc) Default Empty
        verify r
            [ expectResponseCode HTTP.status202
            , expectFieldBetween amount (feeMin + (2*amt), feeMax + (2*amt))
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]
        verify ra
            [ expectFieldBetween balanceTotal
                ( faucetAmt - feeMax - (2*amt)
                , faucetAmt - feeMin - (2*amt)
                )
            , expectFieldEqual balanceAvailable (faucetAmt - 2 * faucetUtxoAmt)
            ]
        rd <- request @ApiWallet ctx (getWalletEp wDest) Default Empty
        verify rd
            [ expectEventually ctx balanceAvailable (2*amt)
            , expectEventually ctx balanceTotal (2*amt)
            ]

    it "TRANS_CREATE_02 - Multiple Output Tx to different wallets" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest1 <- emptyWallet ctx
        wDest2 <- emptyWallet ctx
        addrs1 <- listAddresses ctx wDest1
        addrs2 <- listAddresses ctx wDest2

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
        let (feeMin, feeMax) = ctx ^. feeEstimator $ TxDescription
                { nInputs = 2
                , nOutputs = 2
                }

        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        ra <- request @ApiWallet ctx (getWalletEp wSrc) Default Empty
        verify r
            [ expectResponseCode HTTP.status202
            , expectFieldBetween amount (feeMin + (2*amt), feeMax + (2*amt))
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]
        verify ra
            [ expectFieldBetween balanceTotal
                ( faucetAmt - feeMax - (2*amt)
                , faucetAmt - feeMin - (2*amt)
                )
            , expectFieldEqual balanceAvailable (faucetAmt - 2 * faucetUtxoAmt)
            ]
        forM_ [wDest1, wDest2] $ \wDest -> do
            rd <- request @ApiWallet ctx (getWalletEp wDest) Default payload
            verify rd
                [ expectSuccess
                , expectEventually ctx balanceAvailable amt
                , expectEventually ctx balanceTotal amt
                ]

    it "TRANS_CREATE_02 - Multiple Output Txs don't work on single UTxO" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [2_124_333]
        wDest <- emptyWallet ctx
        addrs <- listAddresses ctx wDest

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

        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403UTxO
            ]

    it "TRANS_CREATE_03 - 0 balance after transaction" $ \ctx -> do
        let (feeMin, _) = ctx ^. feeEstimator $ TxDescription 1 1
        let amt = 1
        wSrc <- fixtureWalletWith ctx [feeMin+amt]
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest

        let destination = addr ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "Secure Passphrase"
            }|]
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectFieldEqual amount (feeMin + amt)
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        ra <- request @ApiWallet ctx (getWalletEp wSrc) Default Empty
        verify ra
            [ expectFieldEqual balanceTotal 0
            , expectFieldEqual balanceAvailable 0
            ]

        rd <- request @ApiWallet ctx (getWalletEp wDest) Default Empty
        verify rd
            [ expectEventually ctx balanceAvailable amt
            , expectEventually ctx balanceTotal amt
            ]

        ra2 <- request @ApiWallet ctx (getWalletEp wSrc) Default Empty
        verify ra2
            [ expectFieldEqual balanceTotal 0
            , expectFieldEqual balanceAvailable 0
            ]

    it "TRANS_CREATE_04 - Error shown when ErrInputsDepleted encountered" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [12_000_000, 20_000_000, 17_000_000]
        wDest <- emptyWallet ctx
        addrs <- listAddresses ctx wDest

        let addrIds = view #id <$> take 3 addrs
        let amounts = [40_000_000, 22, 22] :: [Natural]
        let payments = flip map (zip amounts addrIds) $ \(coin, addr) -> [json|{
                "address": #{addr},
                "amount": {
                    "quantity": #{coin},
                    "unit": "lovelace"
                }
            }|]
        let payload = Json [json|{
                "payments": #{payments :: [Value]},
                "passphrase": "Secure Passphrase"
            }|]
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InputsDepleted
            ]

    it "TRANS_CREATE_04 - Can't cover fee" $ \ctx -> do
        let (feeMin, _) = ctx ^. feeEstimator $ TxDescription 1 1
        wSrc <- fixtureWalletWith ctx [feeMin `div` 2]
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest

        let destination = addr ^. #id
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
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403Fee
            ]

    it "TRANS_CREATE_04 - Not enough money" $ \ctx -> do
        let (feeMin, _) = ctx ^. feeEstimator $ TxDescription 1 1
        wSrc <- fixtureWalletWith ctx [feeMin]
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest

        let destination = addr ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": 1000000,
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage $
                errMsg403NotEnoughMoney (fromIntegral feeMin) 1_000_000
            ]

    it "TRANS_CREATE_04 - Wrong password" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest

        let destination = addr ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": 1,
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "This password is wrong"
            }|]
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    describe "TRANS_CREATE_05 - Invalid addresses" $ do
        forM_ matrixWrongAddrs $ \(title, addr, errMsg) -> it title $ \ctx -> do
            wSrc <- emptyWallet ctx
            let payload = Json [json|{
                    "payments": [{
                        "address": #{addr},
                        "amount": {
                            "quantity": 1,
                            "unit": "lovelace"
                        }
                    }],
                    "passphrase": "cardano-wallet"
                }|]
            r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
            verify r
                [ expectResponseCode HTTP.status400
                , expectErrorMessage errMsg
                ]

    it "TRANS_CREATE_05 - [] as address" $ \ctx -> do
        wSrc <- emptyWallet ctx
        let payload = Json [json|{
                "payments": [{
                    "address": [],
                    "amount": {
                        "quantity": 1,
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "expected Text, encountered Array"
            ]

    it "TRANS_CREATE_05 - Num as address" $ \ctx -> do
        wSrc <- emptyWallet ctx
        let payload = Json [json|{
                "payments": [{
                    "address": 123123,
                    "amount": {
                        "quantity": 1,
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "expected Text, encountered Num"
            ]

    it "TRANS_CREATE_05 - address param missing" $ \ctx -> do
        wSrc <- emptyWallet ctx
        let payload = Json [json|{
                "payments": [{
                    "amount": {
                        "quantity": 1,
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "key 'address' not present"
            ]

    describe "TRANS_CREATE_06 - Invalid amount" $ do
        forM_ (matrixInvalidQuantities @(ApiTransaction t)) $ \(title, amt, expectations) -> it title $ \ctx -> do
            wSrc <- emptyWallet ctx
            wDest <- emptyWallet ctx
            addr:_ <- listAddresses ctx wDest

            let destination = addr ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": #{amt}
                    }],
                    "passphrase": "cardano-wallet"
                }|]
            r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
            verify r expectations

    describe "TRANS_CREATE_07 - False wallet ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            wDest <- emptyWallet ctx
            addr:_ <- listAddresses ctx wDest
            let destination = addr ^. #id
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
            let endpoint = "v2/wallets/" <> walId <> "/transactions"
            r <- request @(ApiTransaction t) ctx ("POST", T.pack endpoint)
                    Default payload
            expectResponseCode HTTP.status404 r
            if (title == "40 chars hex") then
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) r
            else
                expectErrorMessage errMsg404NoEndpoint r

    it "TRANS_CREATE_07 - 'almost' valid walletId" $ \ctx -> do
        w <- emptyWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest
        let destination = addr ^. #id
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
        let endpoint =
                "v2/wallets" <> T.unpack (T.append (w ^. walletId) "0")
                <> "/transactions"
        r <- request @(ApiTransaction t) ctx ("POST", T.pack endpoint)
                Default payload
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage errMsg404NoEndpoint r

    it "TRANS_CREATE_07 - Deleted wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (deleteWalletEp w) Default Empty
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest
        let destination = addr ^. #id
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
        r <- request @(ApiTransaction t) ctx (postTxEp w) Default payload
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "TRANS_CREATE_08 - v2/wallets/{id}/transactions - Methods Not Allowed" $ do
        let matrix = ["PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            w <- emptyWallet ctx
            wDest <- emptyWallet ctx
            addr:_ <- listAddresses ctx wDest
            let destination = addr ^. #id
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
            let endpoint = "v2/wallets/" <> w ^. walletId <> "/transactions"
            r <- request @(ApiTransaction t) ctx (method, endpoint)
                    Default payload
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    describe "TRANS_CREATE_08 - HTTP headers" $ do
        forM_ (matrixHeaders @(ApiTransaction t)) $ \(title, headers, expectations) -> it title $ \ctx -> do
            w <- emptyWallet ctx
            wDest <- emptyWallet ctx
            addr:_ <- listAddresses ctx wDest
            let destination = addr ^. #id
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
            r <- request @(ApiTransaction t) ctx (postTxEp w)
                    headers payload
            verify r expectations

    describe "TRANS_CREATE_08 - Bad payload" $ do
        let matrix =
                [ ( "empty payload", NonJson "" )
                , ( "{} payload", NonJson "{}" )
                , ( "non-json valid payload"
                  , NonJson
                        "{ payments: [{\
                         \\"address\": 12312323,\
                         \\"amount: {\
                         \\"quantity\": 1,\
                         \\"unit\": \"lovelace\"} }],\
                         \\"passphrase\": \"cardano-wallet\" }"
                  )
                ]

        forM_ matrix $ \(name, nonJson) -> it name $ \ctx -> do
            w <- emptyWallet ctx
            let payload = nonJson
            r <- request @(ApiTransaction t) ctx (postTxEp w)
                    Default payload
            expectResponseCode @IO HTTP.status400 r

    describe "TRANS_ESTIMATE_08 - v2/wallets/{id}/transactions/fees - Methods Not Allowed" $ do
        let matrix = ["PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS", "GET"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            w <- emptyWallet ctx
            wDest <- emptyWallet ctx
            addr:_ <- listAddresses ctx wDest
            let destination = addr ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": 1,
                            "unit": "lovelace"
                        }
                    }]
                }|]
            let endpoint = "v2/wallets/" <> w ^. walletId <> "/transactions/fees"
            r <- request @ApiFee ctx (method, endpoint)
                    Default payload
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r


    describe "TRANS_ESTIMATE_08 - HTTP headers" $ do
        forM_ (matrixHeaders @ApiFee) $ \(title, headers, expectations) -> it title $ \ctx -> do
            w <- emptyWallet ctx
            wDest <- emptyWallet ctx
            addr:_ <- listAddresses ctx wDest
            let destination = addr ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": 1,
                            "unit": "lovelace"
                        }
                    }]
                }|]
            r <- request @ApiFee ctx (postTxFeeEp w)
                    headers payload
            verify r expectations


    describe "TRANS_ESTIMATE_08 - Bad payload" $ do
        let matrix =
                [ ( "empty payload", NonJson "" )
                , ( "{} payload", NonJson "{}" )
                , ( "non-json valid payload"
                  , NonJson
                        "{ payments: [{\
                         \\"address\": 12312323,\
                         \\"amount: {\
                         \\"quantity\": 1,\
                         \\"unit\": \"lovelace\"} }]\
                         \ }"
                  )
                ]

        forM_ matrix $ \(name, nonJson) -> it name $ \ctx -> do
            w <- emptyWallet ctx
            let payload = nonJson
            r <- request @ApiFee ctx (postTxFeeEp w)
                    Default payload
            expectResponseCode @IO HTTP.status400 r

    it "TRANS_ESTIMATE_01 - Single Output Fee Estimation" $ \ctx -> do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
        addrs <- listAddresses ctx wb

        let amt = 1
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }]
            }|]
        let (feeMin, feeMax) = ctx ^. feeEstimator $ TxDescription
                { nInputs = 1
                , nOutputs = 1
                }

        r <- request @ApiFee ctx (postTxFeeEp wa) Default payload
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectFieldBetween amount (feeMin - amt, feeMax + amt)
            ]

    it "TRANS_ESTIMATE_02 - Multiple Output Fee Estimation to single wallet" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addrs <- listAddresses ctx wDest

        let amt = 1
        let destination1 = (addrs !! 1) ^. #id
        let destination2 = (addrs !! 2) ^. #id
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
                }]
            }|]
        let (feeMin, feeMax) = ctx ^. feeEstimator $ TxDescription
                { nInputs = 2
                , nOutputs = 2
                }

        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectFieldBetween amount (feeMin - (2*amt), feeMax + (2*amt))
            ]

    it "TRANS_ESTIMATE_02 - Multiple Output Fee Estimation to different wallets" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest1 <- emptyWallet ctx
        wDest2 <- emptyWallet ctx
        addrs1 <- listAddresses ctx wDest1
        addrs2 <- listAddresses ctx wDest2

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
                ]
            }|]
        let (feeMin, feeMax) = ctx ^. feeEstimator $ TxDescription
                { nInputs = 2
                , nOutputs = 2
                }

        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectFieldBetween amount (feeMin - (2*amt), feeMax + (2*amt))
            ]

    it "TRANS_ESTIMATE_02 - Multiple Output Fee Estimation don't work on single UTxO" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [2_124_333]
        wDest <- emptyWallet ctx
        addrs <- listAddresses ctx wDest

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
                ]
            }|]

        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403UTxO
            ]

    it "TRANS_ESTIMATE_03 - we see result when we can't cover fee" $ \ctx -> do
        let (feeMin, feeMax) = ctx ^. feeEstimator $ TxDescription 1 1
        wSrc <- fixtureWalletWith ctx [feeMin `div` 2]
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest
        let amt = 1

        let destination = addr ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }]
            }|]
        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectFieldBetween amount (feeMin - amt, feeMax + amt)
            ]

    it "TRANS_ESTIMATE_04 - Not enough money" $ \ctx -> do
        let (feeMin, _) = ctx ^. feeEstimator $ TxDescription 1 1
        wSrc <- fixtureWalletWith ctx [feeMin]
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest

        let destination = addr ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": 1000000,
                        "unit": "lovelace"
                    }
                }]
            }|]
        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage $
                errMsg403NotEnoughMoney (fromIntegral feeMin) 1_000_000
            ]

    describe "TRANS_ESTIMATE_05 - Invalid addresses" $ do
        forM_ matrixWrongAddrs $ \(title, addr, errMsg) -> it title $ \ctx -> do
            wSrc <- emptyWallet ctx
            let payload = Json [json|{
                    "payments": [{
                        "address": #{addr},
                        "amount": {
                            "quantity": 1,
                            "unit": "lovelace"
                        }
                    }]
                }|]
            r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
            verify r
                [ expectResponseCode HTTP.status400
                , expectErrorMessage errMsg
                ]

    it "TRANS_ESTIMATE_05 - [] as address" $ \ctx -> do
        wSrc <- emptyWallet ctx
        let payload = Json [json|{
                "payments": [{
                    "address": [],
                    "amount": {
                        "quantity": 1,
                        "unit": "lovelace"
                    }
                }]
            }|]
        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "expected Text, encountered Array"
            ]

    it "TRANS_ESTIMATE_05 - Num as address" $ \ctx -> do
        wSrc <- emptyWallet ctx
        let payload = Json [json|{
                "payments": [{
                    "address": 123123,
                    "amount": {
                        "quantity": 1,
                        "unit": "lovelace"
                    }
                }]
            }|]
        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "expected Text, encountered Num"
            ]

    it "TRANS_ESTIMATE_05 - address param missing" $ \ctx -> do
        wSrc <- emptyWallet ctx
        let payload = Json [json|{
                "payments": [{
                    "amount": {
                        "quantity": 1,
                        "unit": "lovelace"
                    }
                }]
            }|]
        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "key 'address' not present"
            ]

    describe "TRANS_ESTIMATE_06 - Invalid amount" $ do
        forM_ (matrixInvalidQuantities @ApiFee) $ \(title, amt, expectations) -> it title $ \ctx -> do
            wSrc <- emptyWallet ctx
            wDest <- emptyWallet ctx
            addr:_ <- listAddresses ctx wDest

            let destination = addr ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": #{amt}
                    }]
                }|]
            r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
            verify r expectations

    describe "TRANS_ESTIMATE_07 - False wallet ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            wDest <- emptyWallet ctx
            addr:_ <- listAddresses ctx wDest
            let destination = addr ^. #id
            let payload = Json [json|{
                    "payments": [{
                        "address": #{destination},
                        "amount": {
                            "quantity": 1,
                            "unit": "lovelace"
                        }
                    }]
                }|]
            let endpoint = "v2/wallets/" <> walId <> "/transactions/fees"
            r <- request @ApiFee ctx ("POST", T.pack endpoint)
                    Default payload
            expectResponseCode HTTP.status404 r
            if (title == "40 chars hex") then
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) r
            else
                expectErrorMessage errMsg404NoEndpoint r

    it "TRANS_ESTIMATE_07 - 'almost' valid walletId" $ \ctx -> do
        w <- emptyWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest
        let destination = addr ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": 1,
                        "unit": "lovelace"
                    }
                }]
            }|]
        let endpoint =
                "v2/wallets" <> T.unpack (T.append (w ^. walletId) "0")
                <> "/transactions/fees"
        r <- request @ApiFee ctx ("POST", T.pack endpoint)
                Default payload
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage errMsg404NoEndpoint r

    it "TRANS_ESTIMATE_07 - Deleted wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (deleteWalletEp w) Default Empty
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest
        let destination = addr ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": 1,
                        "unit": "lovelace"
                    }
                }]
            }|]
        r <- request @ApiFee ctx (postTxFeeEp w) Default payload
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r
  where
    longAddr = replicate 10000 '1'
    encodeErr = "Unable to decode Address:"
    matrixWrongAddrs =
        [ ( "long hex", longAddr, encodeErr )
        , ( "short hex", "1", encodeErr )
        , ( "-1000", "-1000", encodeErr )
        , ( "q", "q", encodeErr )
        , ( "empty", "", encodeErr )
        , ( "wildcards", T.unpack wildcardsWalletName, encodeErr )
        , ( "arabic", T.unpack arabicWalletName, encodeErr )
        , ( "kanji", T.unpack kanjiWalletName, encodeErr )
        , ( "polish", T.unpack polishWalletName, encodeErr )
        ]
    unitErr = "failed to parse quantified value. Expected value in\
              \ 'lovelace' (e.g. { 'unit': 'lovelace', 'quantity': ... }"
    matrixInvalidQuantities
        :: (Show a)
        => [( String
            , Value
            , [(HTTP.Status, Either RequestException a) -> IO ()])
           ]
    matrixInvalidQuantities =
        [ ( "Quantity = 1.5"
        , [json|{"quantity": 1.5, "unit": "lovelace"}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage "expected Natural, encountered\
              \ floating number 1.5" ]
        )
        , ( "Quantity = -1000"
        , [json|{"quantity": -1000, "unit": "lovelace"}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage "expected Natural, encountered\
              \ negative number -1000" ]
        )
        , ( "Quantity = \"-1000\""
        , [json|{"quantity": "-1000", "unit": "lovelace"}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage "expected Natural, encountered String" ]
        )
        , ( "Quantity = []"
        , [json|{"quantity": [], "unit": "lovelace"}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage "expected Natural, encountered Array" ]
        )
        , ( "Quantity = \"string with diacritics\""
        , [json|{"quantity": #{polishWalletName}
                , "unit": "lovelace"}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage "expected Natural, encountered String" ]
        )
        , ( "Quantity = \"string with wildcards\""
        , [json|{"quantity": #{wildcardsWalletName}
                , "unit": "lovelace"}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage "expected Natural, encountered String" ]
        )
        , ( "Quantity missing"
        , [json|{"unit": "lovelace"}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage "key 'quantity' not present" ]
        )
        , ( "Unit missing"
        , [json|{"quantity": 1}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage "key 'unit' not present" ]
        )
        , ( "Unit = [\"lovelace\"]"
        , [json|{"quantity": 1, "unit": ["lovelace"]}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage unitErr ]
        )
        , ( "Unit = -33", [json|{"quantity": 1, "unit": -33}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage unitErr ]
        )
        , ( "Unit = 33", [json|{"quantity": 1, "unit": 33}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage unitErr ]
        )
        , ( "Unit = \"LOVELACE\""
        , [json|{"quantity": 1, "unit": "LOVELACE"}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage unitErr ]
        )
        , ( "Unit = \"ada\"", [json|{"quantity": 1, "unit": "ada"}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage unitErr ]
        )
        ]
    matrixHeaders
        :: (Show a)
        => [( String
            , Headers
            , [(HTTP.Status, Either RequestException a) -> IO ()])
           ]
    matrixHeaders =
        [ ( "No HTTP headers -> 415", None
          , [ expectResponseCode @IO HTTP.status415
           , expectErrorMessage errMsg415 ]
        )
        , ( "Accept: text/plain -> 406"
          , Headers [ ("Content-Type", "application/json")
                    , ("Accept", "text/plain") ]
          , [ expectResponseCode @IO HTTP.status406
            , expectErrorMessage errMsg406 ]
        )
        , ( "No Content-Type -> 415"
          , Headers [ ("Accept", "application/json") ]
          , [ expectResponseCode @IO HTTP.status415
          , expectErrorMessage errMsg415 ]
        )
        , ( "Content-Type: text/plain -> 415"
          , Headers [ ("Content-Type", "text/plain") ]
          , [ expectResponseCode @IO HTTP.status415
            , expectErrorMessage errMsg415 ]
        )
        ]
