{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiFee
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiWallet
    , insertedAt
    , pendingSince
    , time
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( Direction (..), TxStatus (..), WalletId )
import Control.Monad
    ( forM_ )
import Data.Aeson
    ( Value )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Product.Typed
    ( HasType )
import Data.Maybe
    ( isJust )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( UTCTime, addUTCTime )
import Data.Time.Utils
    ( utcTimePred, utcTimeSucc )
import Network.HTTP.Types.Method
    ( Method )
import Network.Wai.Middleware.ServantError
    ( servantErrorMsg )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldSatisfy )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , TxDescription (..)
    , amount
    , balanceAvailable
    , balanceTotal
    , deleteByronTxEp
    , deleteTxEp
    , deleteWalletEp
    , direction
    , emptyByronWallet
    , emptyWallet
    , eventually_
    , expectErrorMessage
    , expectEventually
    , expectEventually'
    , expectFieldBetween
    , expectFieldEqual
    , expectListItemFieldBetween
    , expectListItemFieldEqual
    , expectListSizeEqual
    , expectResponseCode
    , expectSuccess
    , faucetAmt
    , faucetUtxoAmt
    , feeEstimator
    , fixtureByronWallet
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWith
    , getFromResponse
    , getWalletEp
    , json
    , listAddresses
    , listAllTransactions
    , listByronTxEp
    , listTransactions
    , listTxEp
    , migrateByronWalletEp
    , postTxEp
    , postTxFeeEp
    , request
    , status
    , toQueryString
    , unsafeRequest
    , utcIso8601ToText
    , verify
    , walletId
    )
import Test.Integration.Framework.Request
    ( RequestException )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , errMsg400StartTimeLaterThanEndTime
    , errMsg403Fee
    , errMsg403InputsDepleted
    , errMsg403NoPendingAnymore
    , errMsg403NotEnoughMoney
    , errMsg403UTxO
    , errMsg403WrongPass
    , errMsg404CannotFindTx
    , errMsg404NoEndpoint
    , errMsg404NoWallet
    , errMsg405
    , errMsg406
    , errMsg415
    , falseWalletIds
    , getHeaderCases
    , kanjiWalletName
    , polishWalletName
    , wildcardsWalletName
    )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

data TestCase a = TestCase
    { query :: T.Text
    , assertions :: [(HTTP.Status, Either RequestException a) -> IO ()]
    }

spec :: forall t n. (n ~ 'Testnet) => SpecWith (Context t)
spec = do
    it "Regression #1004 -\
        \ Transaction to self shows only fees as a tx amount\
        \ while both, pending and in_ledger" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        let (feeMin, feeMax) = ctx ^. feeEstimator $ PaymentDescription
                { nInputs = 1
                , nOutputs = 1
                , nChanges = 1
                }

        let amt = 1000
        r <- postTx ctx (wSrc, postTxEp, fixturePassphrase) wSrc amt
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            -- tx amount includes only fees because it is tx to self address
            -- when tx is pending
            , expectFieldBetween amount (feeMin, feeMax)
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        eventually_ $ do
            rt <- request @([ApiTransaction n]) ctx
                (listTxEp wSrc mempty)
                Default
                Empty
            verify rt
                [ expectSuccess
                , expectResponseCode HTTP.status200
                -- tx amount includes only fees because it is tx to self address
                -- also when tx is already in ledger
                , expectListItemFieldBetween 0 amount (feeMin, feeMax)
                , expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                ]

    it "Regression #935 -\
        \ Pending tx should have pendingSince in the list tx response" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [5_000_000]
        wDest <- emptyWallet ctx

        eventually_ $ do
            -- Post Tx
            let amt = (1 :: Natural)
            r <- postTx ctx (wSrc, postTxEp ,"Secure Passphrase") wDest amt
            let tx = getFromResponse Prelude.id r
            tx ^. status `shouldBe` Pending
            insertedAt tx `shouldBe` Nothing
            pendingSince tx `shouldSatisfy` isJust

            -- Verify Tx
            let q = toQueryString [("order", "descending")]
            (_, txs) <- unsafeRequest @([ApiTransaction n]) ctx (listTxEp wSrc q) Empty
            case filter ((== Pending) . view status) txs of
                [] ->
                    fail "Tx no longer pending, need to retry scenario."
                tx':_ -> do
                    tx' ^. direction `shouldBe` Outgoing
                    tx' ^. status `shouldBe` Pending
                    insertedAt tx' `shouldBe` Nothing
                    pendingSince tx' `shouldBe` pendingSince tx

    it "TRANS_CREATE_01 - Single Output Transaction" $ \ctx -> do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
        let (feeMin, feeMax) = ctx ^. feeEstimator $ PaymentDescription
                { nInputs = 1
                , nOutputs = 1
                , nChanges = 1
                }
        let amt = (1 :: Natural)
        r <- postTx ctx (wa, postTxEp, "cardano-wallet") wb amt
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
            , expectEventually ctx getWalletEp balanceAvailable (faucetAmt + amt)
            ]

        verify ra
            [ expectEventually ctx getWalletEp balanceAvailable (faucetAmt - feeMax - amt)
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
        let (feeMin, feeMax) = ctx ^. feeEstimator $ PaymentDescription
                { nInputs = 2
                , nOutputs = 2
                , nChanges = 2
                }

        r <- request @(ApiTransaction n) ctx (postTxEp wSrc) Default payload
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
            [ expectEventually ctx getWalletEp balanceAvailable (2*amt)
            , expectEventually ctx getWalletEp balanceTotal (2*amt)
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
        let (feeMin, feeMax) = ctx ^. feeEstimator $ PaymentDescription
                { nInputs = 2
                , nOutputs = 2
                , nChanges = 2
                }

        r <- request @(ApiTransaction n) ctx (postTxEp wSrc) Default payload
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
                , expectEventually ctx getWalletEp balanceAvailable amt
                , expectEventually ctx getWalletEp balanceTotal amt
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

        r <- request @(ApiTransaction n) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403UTxO
            ]

    it "TRANS_CREATE_03 - 0 balance after transaction" $ \ctx -> do
        let (feeMin, _) = ctx ^. feeEstimator $ PaymentDescription 1 1 0
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
        r <- request @(ApiTransaction n) ctx (postTxEp wSrc) Default payload
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
            [ expectEventually ctx getWalletEp balanceAvailable amt
            , expectEventually ctx getWalletEp balanceTotal amt
            ]

        ra2 <- request @ApiWallet ctx (getWalletEp wSrc) Default Empty
        verify ra2
            [ expectFieldEqual balanceTotal 0
            , expectFieldEqual balanceAvailable 0
            ]

    it "TRANS_CREATE_04 - Error shown when ErrInputsDepleted encountered" $ \ctx -> do
        (wSrc, payload) <- fixtureErrInputsDepleted ctx
        r <- request @(ApiTransaction n) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InputsDepleted
            ]

    it "TRANS_CREATE_04 - Can't cover fee" $ \ctx -> do
        let (feeMin, _) = ctx ^. feeEstimator $ PaymentDescription 1 1 1
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
        r <- request @(ApiTransaction n) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403Fee
            ]

    it "TRANS_CREATE_04 - Not enough money" $ \ctx -> do
        let (feeMin, _) = ctx ^. feeEstimator $ PaymentDescription 1 1 1
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
        r <- request @(ApiTransaction n) ctx (postTxEp wSrc) Default payload
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
        r <- request @(ApiTransaction n) ctx (postTxEp wSrc) Default payload
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
            r <- request @(ApiTransaction n) ctx (postTxEp wSrc) Default payload
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
        r <- request @(ApiTransaction n) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status400
            , expectErrorMessage (servantErrorMsg "Text" "Array")
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
        r <- request @(ApiTransaction n) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status400
            , expectErrorMessage (servantErrorMsg "Text" "Num")
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
        r <- request @(ApiTransaction n) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status400
            , expectErrorMessage "key 'address' not present"
            ]

    describe "TRANS_CREATE_06 - Invalid amount" $ do
        forM_ (matrixInvalidQuantities @(ApiTransaction n)) $ \(title, amt, expectations) -> it title $ \ctx -> do
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
            r <- request @(ApiTransaction n) ctx (postTxEp wSrc) Default payload
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
            r <- request @(ApiTransaction n) ctx ("POST", T.pack endpoint)
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
                "v2/wallets/" <> T.unpack (T.append (w ^. walletId) "0")
                <> "/transactions"
        r <- request @(ApiTransaction n) ctx ("POST", T.pack endpoint)
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
        r <- request @(ApiTransaction n) ctx (postTxEp w) Default payload
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe
        "TRANS_CREATE_08, TRANS_LIST_04 - \
        \v2/wallets/{id}/transactions - Methods Not Allowed" $ do

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
            r <- request @(ApiTransaction n) ctx (method, endpoint)
                    Default payload
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    describe "TRANS_CREATE_08 - HTTP headers" $ do
        forM_ (matrixHeaders @(ApiTransaction n)) $ \(title, headers, expectations) -> it title $ \ctx -> do
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
            r <- request @(ApiTransaction n) ctx (postTxEp w)
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
            r <- request @(ApiTransaction n) ctx (postTxEp w)
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
        let (feeMin, feeMax) = ctx ^. feeEstimator $ PaymentDescription
                { nInputs = 1
                , nOutputs = 1
                , nChanges = 1
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
        let (feeMin, feeMax) = ctx ^. feeEstimator $ PaymentDescription
                { nInputs = 2
                , nOutputs = 2
                , nChanges = 2
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
        let (feeMin, feeMax) = ctx ^. feeEstimator $ PaymentDescription
                { nInputs = 2
                , nOutputs = 2
                , nChanges = 2
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
        let (feeMin, feeMax) = ctx ^. feeEstimator $ PaymentDescription 1 1 0
        wSrc <- fixtureWalletWith ctx [feeMin `div` 2]
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest
        let amt = 1 :: Natural

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
            , expectFieldBetween amount (feeMin-amt, feeMax+amt)
            ]

    it "TRANS_ESTIMATE_04 - Not enough money" $ \ctx -> do
        let (feeMin, _) = ctx ^. feeEstimator $ PaymentDescription 1 1 1
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

    it "TRANS_ESTIMATE_04 - Error shown when ErrInputsDepleted encountered" $ \ctx -> do
        (wSrc, payload) <- fixtureErrInputsDepleted ctx
        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InputsDepleted
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
            , expectErrorMessage (servantErrorMsg "Text" "Array")
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
            , expectErrorMessage (servantErrorMsg "Text" "Num")
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

    it "TRANS_LIST_01 - Can list Incoming and Outgoing transactions" $ \ctx -> do
        -- Make tx from fixtureWallet
        (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        addrs <- listAddresses ctx wDest

        let amt = 1 :: Natural
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

        tx <- request @(ApiTransaction n) ctx (postTxEp wSrc) Default payload
        expectResponseCode HTTP.status202 tx
        expectEventually' ctx getWalletEp balanceAvailable amt wDest
        expectEventually' ctx getWalletEp balanceTotal amt wDest

        -- Verify Tx list contains Incoming and Outgoing
        r <- request @([ApiTransaction n]) ctx (listTxEp wSrc mempty)
            Default Empty
        expectResponseCode @IO HTTP.status200 r

        verify r
            [ expectListItemFieldEqual 0 direction Outgoing
            , expectListItemFieldEqual 1 direction Incoming
            ]

    -- This scenario covers the following matrix of cases. Cases were generated
    -- using one of pairwise test cases generation tools available online.
    -- +---+----------+----------+------------+--------------+
    --     |  start   |   end    |   order    |    result    |
    -- +---+----------+----------+------------+--------------+
    --   1 | edge     | edge     | ascending  | 2 ascending  |
    --   2 | edge     | edge + 1 | descending | 2 descending |
    --   3 | edge     | edge - 1 | empty      | 1st one      |
    --   4 | edge     | empty    | empty      | 2 descending |
    --   5 | edge + 1 | edge + 1 | empty      | 2nd one      |
    --   6 | edge + 1 | edge - 1 | empty      | none         |
    --   7 | edge + 1 | empty    | ascending  | 2nd one      |
    --   8 | edge + 1 | edge     | descending | 2nd one      |
    --   9 | edge - 1 | edge - 1 | ascending  | 1st one      |
    --  10 | edge - 1 | empty    | descending | 2 descending |
    --  11 | edge - 1 | edge     | empty      | 2 descending |
    --  12 | edge - 1 | edge + 1 | empty      | 2 descending |
    --  13 | empty    | empty    | empty      | 2 descending |
    --  14 | empty    | edge     | empty      | 2 descending |
    --  15 | empty    | edge + 1 | ascending  | 2 ascending  |
    --  16 | empty    | edge - 1 | descending | 1st one      |
    --  17 | t1       | t1       | empty      | 1st one      |
    --  18 | t2       | t2       | descending | 2nd one      |
    -- +---+----------+----------+------------+--------------+
    it "TRANS_LIST_02,03x - Can limit/order results with start, end and order"
        $ \ctx -> do
        let a1 = sum $ replicate 10 1
        let a2 = sum $ replicate 10 2
        w <- fixtureWalletWith ctx $ mconcat
                [ replicate 10 1
                , replicate 10 2
                ]
        txs <- listAllTransactions ctx w
        let [Just t2, Just t1] = fmap (fmap time . insertedAt) txs
        let matrix :: [TestCase [ApiTransaction n]] =
                [ TestCase -- 1
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText t2)
                        , ("order", "ascending")
                        ]
                    , assertions =
                        [ expectListSizeEqual 2
                        , expectListItemFieldEqual 0 amount a1
                        , expectListItemFieldEqual 1 amount a2
                        ]
                    }
                , TestCase -- 2
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText $ plusOneSecond t2)
                        , ("order", "descending")
                        ]
                    , assertions =
                        [ expectListSizeEqual 2
                        , expectListItemFieldEqual 0 amount a2
                        , expectListItemFieldEqual 1 amount a1
                        ]
                    }
                , TestCase -- 3
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText $ minusOneSecond t2)
                        ]
                    , assertions =
                        [ expectListSizeEqual 1
                        , expectListItemFieldEqual 0 amount a1
                        ]
                    }
                , TestCase -- 4
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1) ]
                    , assertions =
                        [ expectListSizeEqual 2
                        , expectListItemFieldEqual 0 amount a2
                        , expectListItemFieldEqual 1 amount a1
                        ]
                    }
                , TestCase --5
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ plusOneSecond t1)
                        , ("end", utcIso8601ToText $ plusOneSecond t2)
                        ]
                    , assertions =
                        [ expectListSizeEqual 1
                        , expectListItemFieldEqual 0 amount a2
                        ]
                    }
                , TestCase -- 6
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ plusOneSecond t1)
                        , ("end", utcIso8601ToText $ minusOneSecond t2)
                        ]
                    , assertions =
                        [ expectListSizeEqual 0 ]
                    }
                , TestCase -- 7
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ plusOneSecond t1)
                        , ("order", "ascending")
                        ]
                    , assertions =
                        [ expectListSizeEqual 1
                        , expectListItemFieldEqual 0 amount a2
                        ]
                    }
                , TestCase -- 8
                    { query = toQueryString
                        [ ("order", "descending")
                        , ("start", utcIso8601ToText $ plusOneSecond t1)
                        , ("end", utcIso8601ToText t2)
                        ]
                    , assertions =
                        [ expectListSizeEqual 1
                        , expectListItemFieldEqual 0 amount a2
                        ]
                    }
                , TestCase -- 9
                    { query = toQueryString
                        [ ("order", "ascending")
                        , ("start", utcIso8601ToText $ minusOneSecond t1)
                        , ("end", utcIso8601ToText $ minusOneSecond t2)
                        ]
                    , assertions =
                        [ expectListSizeEqual 1
                        , expectListItemFieldEqual 0 amount a1
                        ]
                    }
                , TestCase -- 10
                    { query = toQueryString
                        [ ("order", "descending")
                        , ("start", utcIso8601ToText $ minusOneSecond t1)
                        ]
                    , assertions =
                        [ expectListSizeEqual 2
                        , expectListItemFieldEqual 0 amount a2
                        , expectListItemFieldEqual 1 amount a1
                        ]
                    }
                , TestCase -- 11
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ minusOneSecond t1)
                        , ("end", utcIso8601ToText t2)
                        ]
                    , assertions =
                        [ expectListSizeEqual 2
                        , expectListItemFieldEqual 0 amount a2
                        , expectListItemFieldEqual 1 amount a1
                        ]
                    }
                , TestCase -- 12
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ minusOneSecond t1)
                        , ("end", utcIso8601ToText $ plusOneSecond t2)
                        ]
                    , assertions =
                        [ expectListSizeEqual 2
                        , expectListItemFieldEqual 0 amount a2
                        , expectListItemFieldEqual 1 amount a1
                        ]
                    }
                , TestCase -- 13
                    { query = mempty
                    , assertions =
                        [ expectListSizeEqual 2
                        , expectListItemFieldEqual 0 amount a2
                        , expectListItemFieldEqual 1 amount a1
                        ]
                    }
                , TestCase -- 14
                    { query = toQueryString
                        [ ("end", utcIso8601ToText t2) ]
                    , assertions =
                        [ expectListSizeEqual 2
                        , expectListItemFieldEqual 0 amount a2
                        , expectListItemFieldEqual 1 amount a1
                        ]
                    }
                , TestCase -- 15
                    { query = toQueryString
                        [ ("end", utcIso8601ToText $ plusOneSecond t2) ]
                    , assertions =
                        [ expectListSizeEqual 2
                        , expectListItemFieldEqual 0 amount a2
                        , expectListItemFieldEqual 1 amount a1
                        ]
                    }
                , TestCase -- 16
                    { query = toQueryString
                        [ ("end", utcIso8601ToText $ minusOneSecond t2) ]
                    , assertions =
                        [ expectListSizeEqual 1
                        , expectListItemFieldEqual 0 amount a1
                        ]
                    }
                , TestCase -- 17
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText t1)
                        ]
                    , assertions =
                        [ expectListSizeEqual 1
                        , expectListItemFieldEqual 0 amount a1
                        ]
                    }
                , TestCase -- 18
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t2)
                        , ("end", utcIso8601ToText t2)
                        ]
                    , assertions =
                        [ expectListSizeEqual 1
                        , expectListItemFieldEqual 0 amount a2
                        ]
                    }
                ]

        forM_ matrix $ \tc -> do
          rf <- request @([ApiTransaction n]) ctx (listTxEp w (query tc))
              Default Empty
          verify rf (assertions tc)

    describe "TRANS_LIST_02,03 - Faulty start, end, order values" $ do
        let orderErr = "Please specify one of the following values:\
            \ ascending, descending."
        let startEndErr = "Expecting ISO 8601 date-and-time format\
            \ (basic or extended), e.g. 2012-09-25T10:15:00Z."
        let queries :: [TestCase [ApiTransaction n]] =
                [
                  TestCase
                    { query = toQueryString [ ("start", "2009") ]
                    , assertions =
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                    }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25T10:15:00Z")
                             , ("end", "2016-11-21")
                             ]
                     , assertions =
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25")
                             , ("end", "2016-11-21T10:15:00Z")
                             ]
                     , assertions =
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("end", "2012-09-25T10:15:00Z")
                             , ("start", "2016-11-21")
                             ]
                     , assertions =
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage startEndErr
                             ]

                     }
                 , TestCase
                     { query = toQueryString [ ("order", "scending") ]
                     , assertions =
                            [ expectResponseCode @IO HTTP.status400
                            , expectErrorMessage orderErr
                            ]

                     }
                 , TestCase
                     { query = toQueryString
                             [ ("start", "2012-09-25T10:15:00Z")
                             , ("order", "asc")
                             ]
                     , assertions =
                             [ expectResponseCode @IO HTTP.status400
                             , expectErrorMessage orderErr
                             ]
                     }
                ]

        forM_ queries $ \tc -> it (T.unpack $ query tc) $ \ctx -> do
            w <- emptyWallet ctx
            r <- request @([ApiTransaction n]) ctx (listTxEp w (query tc))
                Default Empty
            verify r (assertions tc)

    it "TRANS_LIST_02 - Start time shouldn't be later than end time" $
        \ctx -> do
              w <- emptyWallet ctx
              let startTime = "2009-09-09T09:09:09Z"
              let endTime = "2001-01-01T01:01:01Z"
              let q = toQueryString
                      [ ("start", T.pack startTime)
                      , ("end", T.pack endTime)
                      ]
              r <- request @([ApiTransaction n]) ctx (listTxEp w q)
                  Default Empty
              expectResponseCode @IO HTTP.status400 r
              expectErrorMessage
                  (errMsg400StartTimeLaterThanEndTime startTime endTime) r
              pure ()

    describe "TRANS_LIST_04 - Request headers" $ do
        let headerCases =
                  [ ( "No HTTP headers -> 200", None
                    , [ expectResponseCode @IO HTTP.status200 ] )
                  , ( "Accept: text/plain -> 406"
                    , Headers
                          [ ("Content-Type", "application/json")
                          , ("Accept", "text/plain") ]
                    , [ expectResponseCode @IO HTTP.status406
                      , expectErrorMessage errMsg406 ]
                    )
                  , ( "No Accept -> 200"
                    , Headers [ ("Content-Type", "application/json") ]
                    , [ expectResponseCode @IO HTTP.status200 ]
                    )
                  , ( "No Content-Type -> 200"
                    , Headers [ ("Accept", "application/json") ]
                    , [ expectResponseCode @IO HTTP.status200 ]
                    )
                  , ( "Content-Type: text/plain -> 200"
                    , Headers [ ("Content-Type", "text/plain") ]
                    , [ expectResponseCode @IO HTTP.status200 ]
                    )
                  ]
        forM_ headerCases $ \(title, headers, expectations) -> it title $ \ctx -> do
            w <- emptyWallet ctx
            r <- request @([ApiTransaction n]) ctx (listTxEp w mempty) headers Empty
            verify r expectations

    it "TRANS_LIST_04 - 'almost' valid walletId" $ \ctx -> do
        w <- emptyWallet ctx
        let endpoint = "v2/wallets/"
                <> T.unpack (T.append (w ^. walletId) "0")
                <> "/transactions"
        r <- request @([ApiTransaction n]) ctx ("GET", T.pack endpoint)
                Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage errMsg404NoEndpoint r

    it "TRANS_LIST_04 - Deleted wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (deleteWalletEp w) Default Empty
        r <- request @([ApiTransaction n]) ctx (listTxEp w mempty)
            Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "TRANS_LIST_04 - False wallet ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            let endpoint = "v2/wallets/" <> walId <> "/transactions"
            r <- request @([ApiTransaction n]) ctx ("GET", T.pack endpoint)
                    Default Empty
            expectResponseCode HTTP.status404 r
            if (title == "40 chars hex") then
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) r
            else
                expectErrorMessage errMsg404NoEndpoint r :: IO ()

    it "TRANS_LIST_RANGE_01 - \
       \Transaction at time t is SELECTED by small ranges that cover it" $
          \ctx -> do
              w <- fixtureWalletWith ctx [1]
              t <- unsafeGetTransactionTime <$> listAllTransactions ctx w
              let (te, tl) = (utcTimePred t, utcTimeSucc t)
              txs1 <- listTransactions ctx w (Just t ) (Just t ) Nothing
              txs2 <- listTransactions ctx w (Just te) (Just t ) Nothing
              txs3 <- listTransactions ctx w (Just t ) (Just tl) Nothing
              txs4 <- listTransactions ctx w (Just te) (Just tl) Nothing
              length <$> [txs1, txs2, txs3, txs4] `shouldSatisfy` all (== 1)

    it "TRANS_LIST_RANGE_02 - \
       \Transaction at time t is NOT selected by range (t + 𝛿t, ...)" $
          \ctx -> do
              w <- fixtureWalletWith ctx [1]
              t <- unsafeGetTransactionTime <$> listAllTransactions ctx w
              let tl = utcTimeSucc t
              txs1 <- listTransactions ctx w (Just tl) (Nothing) Nothing
              txs2 <- listTransactions ctx w (Just tl) (Just tl) Nothing
              length <$> [txs1, txs2] `shouldSatisfy` all (== 0)

    it "TRANS_LIST_RANGE_03 - \
       \Transaction at time t is NOT selected by range (..., t - 𝛿t)" $
          \ctx -> do
              w <- fixtureWalletWith ctx [1]
              t <- unsafeGetTransactionTime <$> listAllTransactions ctx w
              let te = utcTimePred t
              txs1 <- listTransactions ctx w (Nothing) (Just te) Nothing
              txs2 <- listTransactions ctx w (Just te) (Just te) Nothing
              length <$> [txs1, txs2] `shouldSatisfy` all (== 0)

    it "TRANS_DELETE_01 -\
        \ Shelley: Can forget pending transaction" $ \ctx -> do
        (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
        -- post tx
        let amt = (1 :: Natural)
        rMkTx <- postTx ctx (wSrc, postTxEp, "cardano-wallet") wDest amt
        let txid = getFromResponse #id rMkTx
        verify rMkTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectFieldEqual direction Outgoing
            , expectFieldEqual status Pending
            ]

        -- verify balance on src wallet
        request @ApiWallet ctx (getWalletEp wSrc) Default Empty >>= flip verify
            [ expectSuccess
            , expectFieldEqual balanceAvailable (faucetAmt - faucetUtxoAmt)
            ]

        -- forget transaction
        request @ApiTxId ctx (deleteTxEp wSrc (ApiTxId txid)) Default Empty
            >>= expectResponseCode @IO HTTP.status204

        -- verify again balance on src wallet
        request @ApiWallet ctx (getWalletEp wSrc) Default Empty >>= flip verify
            [ expectSuccess
            , expectFieldEqual balanceTotal faucetAmt
            , expectFieldEqual balanceAvailable faucetAmt
            ]

        -- transaction eventually is in source wallet
        eventually_ $ do
            let ep = listTxEp wSrc mempty
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                ]

        -- transaction eventually is in target wallet
        eventually_ $ do
            let ep = listTxEp wDest mempty
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Incoming
                , expectListItemFieldEqual 0 status InLedger
                ]

    it "BYRON_TRANS_DELETE_01 -\
        \ Byron: Can forget pending transaction" $ \ctx -> do
        sourceWallet <- fixtureByronWallet ctx
        targetWallet <- emptyWallet ctx

        -- migrate funds and quickly get id of one of the pending txs
        let payload = Json [json|{"passphrase": #{fixturePassphrase}}|]
        let migrEp = migrateByronWalletEp sourceWallet targetWallet
        (_, t:_) <- unsafeRequest @[ApiTransaction n] ctx migrEp payload
        t ^. status `shouldBe` Pending
        let txid = t ^. #id

        -- quickly forget transaction that is still pending...
        let delEp = deleteByronTxEp sourceWallet (ApiTxId txid)
        rDel <- request @ApiTxId ctx delEp Default Empty
        expectResponseCode @IO HTTP.status204 rDel

    it "TRANS_DELETE_02 -\
        \ Shelley: Cannot forget tx that is already in ledger" $ \ctx -> do
        (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx

        -- post transaction
        rTx <-
            postTx ctx (wSrc, postTxEp, "cardano-wallet") wDest (1 :: Natural)
        let txid = getFromResponse #id rTx

        -- Wait for the transaction to be accepted
        eventually_ $ do
            let ep = listTxEp wSrc mempty
            request @([ApiTransaction n]) ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                ]

        -- Try Forget transaction once it's no longer pending
        let ep = deleteTxEp wSrc (ApiTxId txid)
        rDel <- request @ApiTxId ctx ep Default Empty
        expectResponseCode @IO HTTP.status403 rDel
        let err = errMsg403NoPendingAnymore (toUrlPiece (ApiTxId txid))
        expectErrorMessage err rDel

    it "BYRON_TRANS_DELETE_02 -\
        \ Byron: Cannot forget tx that is already in ledger" $ \ctx -> do
        w <- fixtureByronWallet ctx

        -- Get TX id
        let listEp = listByronTxEp w mempty
        (_, t:_) <- unsafeRequest @([ApiTransaction n]) ctx listEp Empty
        let txid = t ^. #id

        -- Try Forget transaction that is no longer pending
        let delEp = deleteByronTxEp w (ApiTxId txid)
        rDel <- request @ApiTxId ctx delEp Default Empty
        expectResponseCode @IO HTTP.status403 rDel
        let err = errMsg403NoPendingAnymore (toUrlPiece (ApiTxId txid))
        expectErrorMessage err rDel

    describe "TRANS_DELETE_03 - checking no transaction id error for " $ do
        txDeleteNotExistsingTxIdTest emptyWallet "wallets"
        txDeleteNotExistsingTxIdTest emptyByronWallet "byron-wallets"

    describe "TRANS_DELETE_04 - False wallet ids for " $ do
        txDeleteFalseWalletIdsTest "wallets"
        txDeleteFalseWalletIdsTest "byron-wallets"

    describe "TRANS_DELETE_07 - invalid tx id " $ do
        txDeleteInvalidTxIdsTest emptyWallet "wallets"
        txDeleteInvalidTxIdsTest emptyByronWallet "byron-wallets"

    describe "TRANS_DELETE_08 - HTTP headers " $ do
        txDeleteHTTPHeadersTest emptyWallet "wallets"
        txDeleteHTTPHeadersTest emptyByronWallet "byron-wallets"

    describe "TRANS_DELETE_09 - HTTP methods not allowed " $ do
        txDeleteHTTPMethodsTest "wallets"
        txDeleteHTTPMethodsTest "byron-wallets"

    describe "TRANS_DELETE_06 -\
        \ Cannot forget tx that is performed from different wallet" $ do
        txDeleteFromDifferentWalletTest emptyWallet "wallets"
        txDeleteFromDifferentWalletTest emptyByronWallet "byron-wallets"

    it "BYRON_TRANS_DELETE -\
        \ Cannot delete tx on Byron wallet using shelley ep" $ \ctx -> do
            w <- emptyByronWallet ctx
            let wid = w ^. walletId
            let txid = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
            let endpoint = "v2/wallets/" <> wid <> "/transactions/" <> txid
            r <- request @ApiTxId @IO ctx ("DELETE", endpoint) Default Empty
            expectResponseCode HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_TRANS_ESTIMATE -\
        \ Cannot estimate tx on Byron wallet using shelley ep" $ \ctx -> do
            w <- emptyByronWallet ctx
            let wid = w ^. walletId
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
            let endpoint = "v2/wallets/" <> wid <> "/transactions/fees"
            r <- request @ApiFee ctx ("POST", endpoint) Default payload
            expectResponseCode @IO HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_TRANS_CREATE -\
        \ Cannot create tx on Byron wallet using shelley ep" $ \ctx -> do
            w <- emptyByronWallet ctx
            let wid = w ^. walletId
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
            let endpoint = "v2/wallets/" <> wid <> "/transactions"
            r <- request @(ApiTransaction n) ctx ("POST", endpoint) Default payload
            expectResponseCode @IO HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet wid) r
  where
    txDeleteNotExistsingTxIdTest eWallet resource =
        it resource $ \ctx -> do
            w <- eWallet ctx
            let walId = w ^. walletId
            let txid = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
            let endpoint = "v2/" <> T.pack resource <> "/" <> walId <> "/transactions/" <> txid
            ra <- request @ApiTxId @IO ctx ("DELETE", endpoint) Default Empty
            expectResponseCode @IO HTTP.status404 ra
            expectErrorMessage (errMsg404CannotFindTx txid) ra

    txDeleteFalseWalletIdsTest resource =
        describe resource $ do
            forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
                let txid = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
                let endpoint = "v2/" <> resource <> "/" <> walId <> "/transactions/" <> txid
                r <- request @ApiTxId @IO ctx ("DELETE", T.pack endpoint) Default Empty
                expectResponseCode HTTP.status404 r
                if (title == "40 chars hex") then
                    expectErrorMessage (errMsg404NoWallet $ T.pack walId) r
                else
                    expectErrorMessage errMsg404NoEndpoint r

    txDeleteInvalidTxIdsTest eWallet resource =
        describe resource $ do
            let txIds =
                    [ replicate 63 '1'
                    , replicate 65 '1'
                    , replicate 64 'ś'
                    ]
            forM_ txIds $ \tid -> it (show tid) $ \ctx -> do
                w <- eWallet ctx
                let wid = w ^. walletId
                let ep = "v2/" <> T.pack resource <> "/" <> wid
                        <> "/transactions/" <> T.pack tid
                r <- request @ApiTxId @IO ctx ("DELETE", ep) Default Empty
                expectResponseCode @IO HTTP.status404 r
                expectErrorMessage errMsg404NoEndpoint r

    txDeleteFromDifferentWalletTest
        :: (HasType (ApiT WalletId) wal)
        => (Context t -> IO wal)
        -> String
        -> SpecWith (Context t)
    txDeleteFromDifferentWalletTest eWallet resource =
        it resource $ \ctx -> do
            -- post tx
            (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            rMkTx <- postTx ctx (wSrc, postTxEp, "cardano-wallet") wDest (1 :: Natural)

            -- try to forget from different wallet
            wDifferent <- eWallet ctx
            let txid = toText $ getApiT $ getFromResponse #id rMkTx
            let endpoint = "v2/" <> T.pack resource <> "/"
                     <> wDifferent ^. walletId
                     <> "/transactions/"
                     <> txid
            ra <- request @ApiTxId @IO ctx ("DELETE", endpoint) Default Empty
            expectResponseCode @IO HTTP.status404 ra
            expectErrorMessage (errMsg404CannotFindTx txid) ra

    txDeleteHTTPHeadersTest eWallet resource =
        describe resource $ do
            forM_ (getHeaderCases HTTP.status404)
                $ \(title, headers, expectations) -> it title $ \ctx -> do
                w <- eWallet ctx
                let wid = w ^. walletId
                let txid = T.pack $ replicate 64 '1'
                let ep = "v2/" <> T.pack resource <> "/" <> wid
                        <> "/transactions/" <> txid
                r <- request @ApiTxId @IO ctx ("DELETE", ep) headers Empty
                verify r expectations

    txDeleteHTTPMethodsTest res =
        describe ("v2/" <> res <> "/{wid}/transactions/{tid}") $ do
                let matrix =
                        ["POST", "CONNECT", "TRACE", "OPTIONS", "PUT", "GET"]
                forM_ matrix $ \m -> it (show m) $ \ctx -> do
                    let txid = T.pack $ replicate 64 '1'
                    let wid = T.pack $ replicate 40 '1'
                    let ep = "v2/" <> T.pack res <> "/" <> wid
                            <> "/transactions/" <> txid
                    r <- request @ApiTxId @IO ctx (m, ep) Default Empty
                    expectResponseCode @IO HTTP.status405 r
                    expectErrorMessage errMsg405 r

    postTx
        :: Context t
        -> (wal, wal -> (Method, Text), Text)
        -> ApiWallet
        -> Natural
        -> IO (HTTP.Status, Either RequestException (ApiTransaction n))
    postTx ctx (wSrc, postTxEndp, pass) wDest amt = do
        addrs <- listAddresses ctx wDest
        let destination = (addrs !! 1) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{pass}
            }|]
        r <- request @(ApiTransaction n) ctx (postTxEndp wSrc) Default payload
        expectResponseCode HTTP.status202 r
        return r

    unsafeGetTransactionTime
        :: [ApiTransaction n]
        -> UTCTime
    unsafeGetTransactionTime txs =
        case fmap time . insertedAt <$> txs of
            (Just t):_ -> t
            _ -> error "Expected at least one transaction with a time."

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
          , expectErrorMessage (servantErrorMsg "Natural" "floating number 1.5")]
        )
        , ( "Quantity = -1000"
        , [json|{"quantity": -1000, "unit": "lovelace"}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage (servantErrorMsg "Natural" "negative number -1000")]
        )
        , ( "Quantity = \"-1000\""
        , [json|{"quantity": "-1000", "unit": "lovelace"}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage (servantErrorMsg "Natural"  "String")]
        )
        , ( "Quantity = []"
        , [json|{"quantity": [], "unit": "lovelace"}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage (servantErrorMsg "Natural" "Array")]
        )
        , ( "Quantity = \"string with diacritics\""
        , [json|{"quantity": #{polishWalletName}
                , "unit": "lovelace"}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage (servantErrorMsg "Natural" "String")]
        )
        , ( "Quantity = \"string with wildcards\""
        , [json|{"quantity": #{wildcardsWalletName}
                , "unit": "lovelace"}|]
        , [ expectResponseCode HTTP.status400
          , expectErrorMessage (servantErrorMsg "Natural" "String")]
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

    fixtureErrInputsDepleted ctx = do
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
        return (wSrc, payload)

    plusOneSecond, minusOneSecond :: UTCTime -> UTCTime
    plusOneSecond = addUTCTime 1
    minusOneSecond = addUTCTime (-1)
