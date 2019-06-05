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
    ( ApiTransaction, ApiWallet )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress (..), Direction (..), EncodeAddress (..), TxStatus (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , amount
    , balanceAvailable
    , balanceTotal
    , deleteWallet
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
    , getWallet
    , json
    , listAddresses
    , postTx
    , request
    , status
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , errMsg403Fee
    , errMsg403NotEnoughMoney
    , errMsg403UTxO
    , errMsg403WrongPass
    , errMsg404NoEndpoint
    , errMsg404NoWallet
    , errMsg405
    , errMsg406
    , errMsg415
    , errMsg500
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
        let (feeMin, feeMax) = (168609, 168785)

        r <- request @(ApiTransaction t) ctx (postTx wa) Default payload
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
        let (feeMin, feeMax) = (181487, 181839)

        r <- request @(ApiTransaction t) ctx (postTx wSrc) Default payload
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
        let (feeMin, feeMax) = (181487, 181839)

        r <- request @(ApiTransaction t) ctx (postTx wSrc) Default payload
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

        r <- request @(ApiTransaction t) ctx (postTx wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403UTxO
            ]

    it "TRANS_CREATE_03 - 0 balance after transaction" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [168_434]
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
                "passphrase": "Secure Passphrase"
            }|]
        r <- request @(ApiTransaction t) ctx (postTx wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectFieldEqual amount 168434
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        ra <- request @ApiWallet ctx (getWallet wSrc) Default Empty
        verify ra
            [ expectFieldEqual balanceTotal 0
            , expectFieldEqual balanceAvailable 0
            ]

        rd <- request @ApiWallet ctx (getWallet wDest) Default Empty
        verify rd
            [ expectEventually ctx balanceAvailable 1
            , expectEventually ctx balanceTotal 1
            ]

        ra2 <- request @ApiWallet ctx (getWallet wSrc) Default Empty
        verify ra2
            [ expectFieldEqual balanceTotal 0
            , expectFieldEqual balanceAvailable 0
            ]

    it "TRANS_CREATE_04 - Can't cover fee" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [100_000]
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
        r <- request @(ApiTransaction t) ctx (postTx wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403Fee
            ]

    it "TRANS_CREATE_04 - Not enough money" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [100_000]
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
        r <- request @(ApiTransaction t) ctx (postTx wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage (errMsg403NotEnoughMoney 100_000 1000_000)
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
        r <- request @(ApiTransaction t) ctx (postTx wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    describe "TRANS_CREATE_05 - Invalid addresses" $ do
        let longAddr = replicate 10000 '1'
        let byronErr = "Unable to decode Address: not a valid Byron address."
        let base58Err = "Unable to decode Address: expected Base58 encoding."
        let matrix =
                [ ( "long hex", longAddr, byronErr )
                , ( "short hex", "1", byronErr )
                , ( "-1000", "-1000", base58Err ), ( "q", "q", byronErr )
                , ( "empty", "", byronErr )
                , ( "wildcards", T.unpack wildcardsWalletName, base58Err )
                , ( "arabic", T.unpack arabicWalletName, base58Err )
                , ( "kanji", T.unpack kanjiWalletName, base58Err )
                , ( "polish", T.unpack polishWalletName, base58Err )
                ]
        forM_ matrix $ \(title, addr, errMsg) -> it title $ \ctx -> do
            wSrc <- fixtureWallet ctx
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
            r <- request @(ApiTransaction t) ctx (postTx wSrc) Default payload
            verify r
                [ expectResponseCode HTTP.status400
                , expectErrorMessage errMsg
                ]

    it "TRANS_CREATE_05 - [] as address" $ \ctx -> do
        wSrc <- fixtureWallet ctx
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
        r <- request @(ApiTransaction t) ctx (postTx wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "expected Text, encountered Array"
            ]

    it "TRANS_CREATE_05 - Num as address" $ \ctx -> do
        wSrc <- fixtureWallet ctx
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
        r <- request @(ApiTransaction t) ctx (postTx wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "expected Text, encountered Num"
            ]

    it "TRANS_CREATE_05 - address param missing" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        let payload = Json [json|{
                "payments": [{
                    "amount": {
                        "quantity": 1,
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        r <- request @(ApiTransaction t) ctx (postTx wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "key 'address' not present"
            ]

    describe "TRANS_CREATE_06 - Invalid amount" $ do
        let unitErr = "failed to parse quantified value. Expected value in\
            \ 'lovelace' (e.g. { 'unit': 'lovelace', 'quantity': ... }"
        let matrix =
                [ ( "Quantity = 0"
                , [json|{"quantity": 0, "unit": "lovelace"}|]
                , [ expectResponseCode HTTP.status500
                  , expectErrorMessage errMsg500 ] -- TODO change after #364
                )
                , ( "Quantity = 1.5"
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
        forM_ matrix $ \(title, amt, expectations) -> it title $ \ctx -> do
            wSrc <- fixtureWallet ctx
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
            r <- request @(ApiTransaction t) ctx (postTx wSrc) Default payload
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
        w <- fixtureWallet ctx
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
        w <- fixtureWallet ctx
        _ <- request @ApiWallet ctx (deleteWallet w) Default Empty
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
        r <- request @(ApiTransaction t) ctx (postTx w) Default payload
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "TRANS_CREATE_08 - v2/wallets/{id}/transactions - Methods Not Allowed" $ do
        let matrix = ["PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS", "GET"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            w <- fixtureWallet ctx
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
        let matrix =
                 [ ( "No HTTP headers -> 415", None
                   , [ expectResponseCode @IO HTTP.status415
                     , expectErrorMessage errMsg415 ]
                   )
                 , ( "Accept: text/plain -> 406"
                   , Headers
                         [ ("Content-Type", "application/json")
                         , ("Accept", "text/plain") ]
                   , [ expectResponseCode @IO HTTP.status406
                     , expectErrorMessage errMsg406 ]
                   )
                 , ( "No Accept -> 202"
                   , Headers [ ("Content-Type", "application/json") ]
                   , [ expectResponseCode @IO HTTP.status202 ]
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
        forM_ matrix $ \(title, headers, expectations) -> it title $ \ctx -> do
            w <- fixtureWallet ctx
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
            r <- request @(ApiTransaction t) ctx (postTx w)
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
            w <- fixtureWallet ctx
            let payload = nonJson
            r <- request @(ApiTransaction t) ctx (postTx w)
                    Default payload
            expectResponseCode @IO HTTP.status400 r
