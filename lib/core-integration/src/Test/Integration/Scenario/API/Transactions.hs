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
    , WalletStyle (..)
    , insertedAt
    , pendingSince
    , time
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( Direction (..), SortOrder (..), TxStatus (..), WalletId )
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
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Data.Time.Clock
    ( UTCTime, addUTCTime )
import Data.Time.Utils
    ( utcTimePred, utcTimeSucc )
import Network.HTTP.Types.Method
    ( Method )
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
    , between
    , emptyRandomWallet
    , emptyWallet
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , expectSuccess
    , faucetAmt
    , faucetUtxoAmt
    , fixturePassphrase
    , fixtureRandomWallet
    , fixtureWallet
    , fixtureWalletWith
    , getFromResponse
    , json
    , listAddresses
    , listAllTransactions
    , listTransactions
    , request
    , toQueryString
    , unsafeRequest
    , utcIso8601ToText
    , verify
    , walletId
    )
import Test.Integration.Framework.Request
    ( RequestException )
import Test.Integration.Framework.TestData
    ( errMsg400StartTimeLaterThanEndTime
    , errMsg403Fee
    , errMsg403InputsDepleted
    , errMsg403NoPendingAnymore
    , errMsg403NotEnoughMoney
    , errMsg403UTxO
    , errMsg403WrongPass
    , errMsg404CannotFindTx
    , errMsg404NoWallet
    )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Cardano.Wallet.Api.Link as Link
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
        let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
                { nInputs = 1
                , nOutputs = 1
                , nChanges = 1
                }

        let amt = 1000
        r <- postTx ctx
            (wSrc, Link.createTransaction @'Shelley, fixturePassphrase)
            wSrc
            amt
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            -- tx amount includes only fees because it is tx to self address
            -- when tx is pending
            , expectField (#amount . #getQuantity) $ between (feeMin, feeMax)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        eventually "Tx is in ledger" $ do
            rt <- request @([ApiTransaction n]) ctx
                (Link.listTransactions @'Shelley wSrc) Default Empty
            verify rt
                [ expectSuccess
                , expectResponseCode HTTP.status200
                -- tx amount includes only fees because it is tx to self address
                -- also when tx is already in ledger
                , expectListField 0 (#amount . #getQuantity) $ between (feeMin, feeMax)
                , expectListField 0 (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0 (#status . #getApiT) (`shouldBe` InLedger)
                ]

    it "Regression #935 -\
        \ Pending tx should have pendingSince in the list tx response" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [5_000_000]
        wDest <- emptyWallet ctx

        eventually "Pending tx has pendingSince field" $ do
            -- Post Tx
            let amt = (1 :: Natural)
            r <- postTx ctx
                (wSrc, Link.createTransaction @'Shelley,"Secure Passphrase")
                wDest
                amt
            let tx = getFromResponse Prelude.id r
            tx ^. (#status . #getApiT) `shouldBe` Pending
            insertedAt tx `shouldBe` Nothing
            pendingSince tx `shouldSatisfy` isJust

            -- Verify Tx
            let link = Link.listTransactions' @'Shelley wSrc
                    Nothing
                    Nothing
                    (Just Descending)
            (_, txs) <- unsafeRequest @([ApiTransaction n]) ctx link Empty
            case filter ((== Pending) . view (#status . #getApiT)) txs of
                [] ->
                    fail "Tx no longer pending, need to retry scenario."
                tx':_ -> do
                    tx' ^. (#direction . #getApiT) `shouldBe` Outgoing
                    tx' ^. (#status . #getApiT) `shouldBe` Pending
                    insertedAt tx' `shouldBe` Nothing
                    pendingSince tx' `shouldBe` pendingSince tx

    it "TRANS_CREATE_01 - Single Output Transaction" $ \ctx -> do
        (wa, wb) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
        let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
                { nInputs = 1
                , nOutputs = 1
                , nChanges = 1
                }
        let amt = (1 :: Natural)
        r <- postTx ctx
            (wa, Link.createTransaction @'Shelley, "cardano-wallet")
            wb
            amt
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (feeMin + amt, feeMax + amt)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wa) Default Empty
        verify ra
            [ expectSuccess
            , expectField (#balance . #getApiT . #total) $
                between
                    ( Quantity (faucetAmt - feeMax - amt)
                    , Quantity (faucetAmt - feeMin - amt)
                    )
            , expectField
                    (#balance . #getApiT . #available)
                    (`shouldBe` Quantity (faucetAmt - faucetUtxoAmt))
            ]

        eventually "wa and wb balances are as expected" $ do
            rb <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wb) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity (faucetAmt + amt)) rb

            ra2 <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wa) Default Empty
            expectField
                (#balance . #getApiT . #available)
                (`shouldBe` Quantity (faucetAmt - feeMax - amt)) ra2

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
        let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
                { nInputs = 2
                , nOutputs = 2
                , nChanges = 2
                }

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty
        verify r
            [ expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (feeMin + (2*amt), feeMax + (2*amt))
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]
        verify ra
            [ expectField (#balance . #getApiT . #total) $
                between
                    ( Quantity (faucetAmt - feeMax - (2*amt))
                    , Quantity (faucetAmt - feeMin - (2*amt))
                    )
            , expectField
                    (#balance . #getApiT . #available)
                    (`shouldBe` Quantity (faucetAmt - 2 * faucetUtxoAmt))
            ]
        eventually "wDest balance is as expected" $ do
            rd <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rd
                [ expectField
                        (#balance . #getApiT . #available)
                        (`shouldBe` Quantity (2*amt))
                , expectField
                        (#balance . #getApiT . #total)
                        (`shouldBe` Quantity (2*amt))
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
        let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
                { nInputs = 2
                , nOutputs = 2
                , nChanges = 2
                }

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty
        verify r
            [ expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (feeMin + (2*amt), feeMax + (2*amt))
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]
        verify ra
            [ expectField (#balance . #getApiT . #total . #getQuantity) $
                between
                    ( faucetAmt - feeMax - (2*amt)
                    , faucetAmt - feeMin - (2*amt)
                    )
            , expectField
                    (#balance . #getApiT . #available)
                    (`shouldBe` Quantity (faucetAmt - 2 * faucetUtxoAmt))
            ]
        eventually "Balances are as expected" $ forM_ [wDest1, wDest2] $ \wDest -> do
            rd <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default payload
            verify rd
                [ expectSuccess
                , expectField
                        (#balance . #getApiT . #available)
                        (`shouldBe` Quantity amt)
                , expectField
                        (#balance . #getApiT . #total)
                        (`shouldBe` Quantity amt)
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

        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403UTxO
            ]

    it "TRANS_CREATE_03 - 0 balance after transaction" $ \ctx -> do
        let (feeMin, _) = ctx ^. #_feeEstimator $ PaymentDescription 1 1 0
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
        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) (`shouldBe` feeMin + amt)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        ra <- request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty
        verify ra
            [ expectField (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
            , expectField (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            ]

        eventually "Wallet balance is as expected" $ do
            rd <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rd
                [ expectField
                        (#balance . #getApiT . #available)
                        (`shouldBe` Quantity amt)
                , expectField
                        (#balance . #getApiT . #total)
                        (`shouldBe` Quantity amt)
                ]

        ra2 <- request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty
        verify ra2
            [ expectField (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
            , expectField (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            ]

    it "TRANS_CREATE_04 - Error shown when ErrInputsDepleted encountered" $ \ctx -> do
        (wSrc, payload) <- fixtureErrInputsDepleted ctx
        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InputsDepleted
            ]

    it "TRANS_CREATE_04 - Can't cover fee" $ \ctx -> do
        let (feeMin, _) = ctx ^. #_feeEstimator $ PaymentDescription 1 1 1
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
        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403Fee
            ]

    it "TRANS_CREATE_04 - Not enough money" $ \ctx -> do
        let (feeMin, _) = ctx ^. #_feeEstimator $ PaymentDescription 1 1 1
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
        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
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
        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    it "TRANS_CREATE_07 - Deleted wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
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
        r <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley w) Default payload
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

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
            r <- request @(ApiTransaction n) ctx
                (Link.createTransaction @'Shelley w) Default payload
            expectResponseCode @IO HTTP.status400 r

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
            r <- request @ApiFee ctx
                (Link.getTransactionFee @'Shelley w) Default payload
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
        let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
                { nInputs = 1
                , nOutputs = 1
                , nChanges = 1
                }

        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wa) Default payload
        verify r
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (feeMin - amt, feeMax + amt)
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
        let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
                { nInputs = 2
                , nOutputs = 2
                , nChanges = 2
                }

        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (feeMin - (2*amt), feeMax + (2*amt))
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
        let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
                { nInputs = 2
                , nOutputs = 2
                , nChanges = 2
                }

        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (feeMin - (2*amt), feeMax + (2*amt))
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

        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403UTxO
            ]

    it "TRANS_ESTIMATE_03 - we see result when we can't cover fee" $ \ctx -> do
        let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription 1 1 0
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
        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status202
            , expectField (#amount . #getQuantity) $
                between (feeMin - amt, feeMax + amt)
            ]

    it "TRANS_ESTIMATE_04 - Not enough money" $ \ctx -> do
        let (feeMin, _) = ctx ^. #_feeEstimator $ PaymentDescription 1 1 1
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
        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage $
                errMsg403NotEnoughMoney (fromIntegral feeMin) 1_000_000
            ]

    it "TRANS_ESTIMATE_04 - Error shown when ErrInputsDepleted encountered" $ \ctx -> do
        (wSrc, payload) <- fixtureErrInputsDepleted ctx
        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InputsDepleted
            ]

    it "TRANS_ESTIMATE_07 - Deleted wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
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
        r <- request @ApiFee ctx
            (Link.getTransactionFee @'Shelley w) Default payload
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

        tx <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley wSrc) Default payload
        expectResponseCode HTTP.status202 tx
        eventually "Wallet balance is as expected" $ do
            rGet <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wDest) Default Empty
            verify rGet
                [ expectField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity amt)
                , expectField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity amt)
                ]

        -- Verify Tx list contains Incoming and Outgoing
        let link = Link.listTransactions @'Shelley wSrc
        r <- request @([ApiTransaction n]) ctx link Default Empty
        expectResponseCode @IO HTTP.status200 r

        verify r
            [ expectListField 0 (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectListField 1 (#direction . #getApiT) (`shouldBe` Incoming)
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
        let a1 = Quantity $ sum $ replicate 10 1
        let a2 = Quantity $ sum $ replicate 10 2
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
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a1)
                        , expectListField 1 #amount (`shouldBe` a2)
                        ]
                    }
                , TestCase -- 2
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText $ plusOneSecond t2)
                        , ("order", "descending")
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 3
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText $ minusOneSecond t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 4
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1) ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase --5
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ plusOneSecond t1)
                        , ("end", utcIso8601ToText $ plusOneSecond t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a2)
                        ]
                    }
                , TestCase -- 6
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ plusOneSecond t1)
                        , ("end", utcIso8601ToText $ minusOneSecond t2)
                        ]
                    , assertions =
                        [ expectListSize 0 ]
                    }
                , TestCase -- 7
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ plusOneSecond t1)
                        , ("order", "ascending")
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a2)
                        ]
                    }
                , TestCase -- 8
                    { query = toQueryString
                        [ ("order", "descending")
                        , ("start", utcIso8601ToText $ plusOneSecond t1)
                        , ("end", utcIso8601ToText t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a2)
                        ]
                    }
                , TestCase -- 9
                    { query = toQueryString
                        [ ("order", "ascending")
                        , ("start", utcIso8601ToText $ minusOneSecond t1)
                        , ("end", utcIso8601ToText $ minusOneSecond t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 10
                    { query = toQueryString
                        [ ("order", "descending")
                        , ("start", utcIso8601ToText $ minusOneSecond t1)
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 11
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ minusOneSecond t1)
                        , ("end", utcIso8601ToText t2)
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 12
                    { query = toQueryString
                        [ ("start", utcIso8601ToText $ minusOneSecond t1)
                        , ("end", utcIso8601ToText $ plusOneSecond t2)
                        ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 13
                    { query = mempty
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 14
                    { query = toQueryString
                        [ ("end", utcIso8601ToText t2) ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 15
                    { query = toQueryString
                        [ ("end", utcIso8601ToText $ plusOneSecond t2) ]
                    , assertions =
                        [ expectListSize 2
                        , expectListField 0 #amount (`shouldBe` a2)
                        , expectListField 1 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 16
                    { query = toQueryString
                        [ ("end", utcIso8601ToText $ minusOneSecond t2) ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 17
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t1)
                        , ("end", utcIso8601ToText t1)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a1)
                        ]
                    }
                , TestCase -- 18
                    { query = toQueryString
                        [ ("start", utcIso8601ToText t2)
                        , ("end", utcIso8601ToText t2)
                        ]
                    , assertions =
                        [ expectListSize 1
                        , expectListField 0 #amount (`shouldBe` a2)
                        ]
                    }
                ]

        let withQuery q (method, link) = (method, link <> q)

        forM_ matrix $ \tc -> do
            let link = withQuery (query tc) $ Link.listTransactions @'Shelley w
            rf <- request @([ApiTransaction n]) ctx link Default Empty
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

        let withQuery q (method, link) = (method, link <> q)

        forM_ queries $ \tc -> it (T.unpack $ query tc) $ \ctx -> do
            w <- emptyWallet ctx
            let link = withQuery (query tc) $ Link.listTransactions @'Shelley w
            r <- request @([ApiTransaction n]) ctx link Default Empty
            verify r (assertions tc)

    it "TRANS_LIST_02 - Start time shouldn't be later than end time" $
        \ctx -> do
            w <- emptyWallet ctx
            let startTime = "2009-09-09T09:09:09Z"
            let endTime = "2001-01-01T01:01:01Z"
            let link = Link.listTransactions' @'Shelley w
                    (either (const Nothing) Just $ fromText $ T.pack startTime)
                    (either (const Nothing) Just $ fromText $ T.pack endTime)
                    Nothing
            r <- request @([ApiTransaction n]) ctx link Default Empty
            expectResponseCode @IO HTTP.status400 r
            expectErrorMessage
                (errMsg400StartTimeLaterThanEndTime startTime endTime) r
            pure ()

    it "TRANS_LIST_04 - Deleted wallet" $ \ctx -> do
        w <- emptyWallet ctx
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        r <- request @([ApiTransaction n]) ctx (Link.listTransactions @'Shelley w)
            Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

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
        rMkTx <- postTx ctx
            (wSrc, Link.createTransaction @'Shelley, "cardano-wallet")
            wDest
            amt
        let txid = getFromResponse #id rMkTx
        verify rMkTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            ]

        -- verify balance on src wallet
        request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty >>= flip verify
            [ expectSuccess
            , expectField
                    (#balance . #getApiT . #available)
                    (`shouldBe` Quantity (faucetAmt - faucetUtxoAmt))
            ]

        -- forget transaction
        request @ApiTxId ctx (Link.deleteTransaction @'Shelley wSrc (ApiTxId txid)) Default Empty
            >>= expectResponseCode @IO HTTP.status204

        -- verify again balance on src wallet
        request @ApiWallet ctx (Link.getWallet @'Shelley wSrc) Default Empty >>= flip verify
            [ expectSuccess
            , expectField
                    (#balance . #getApiT . #total)
                    (`shouldBe` Quantity faucetAmt)
            , expectField
                    (#balance . #getApiT . #available)
                    (`shouldBe` Quantity faucetAmt)
            ]

        eventually "transaction eventually is in source wallet" $ do
            let ep = Link.listTransactions @'Shelley wSrc
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        eventually "transaction eventually is in target wallet" $ do
            let ep = Link.listTransactions @'Shelley wDest
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Incoming)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

    it "BYRON_TRANS_DELETE_01 -\
        \ Byron: Can forget pending transaction" $ \ctx -> do
        sourceWallet <- fixtureRandomWallet ctx
        targetWallet <- emptyWallet ctx

        -- migrate funds and quickly get id of one of the pending txs
        let payload = Json [json|{"passphrase": #{fixturePassphrase}}|]
        let migrEp = Link.migrateWallet sourceWallet targetWallet
        (_, t:_) <- unsafeRequest @[ApiTransaction n] ctx migrEp payload
        t ^. (#status . #getApiT) `shouldBe` Pending

        -- quickly forget transaction that is still pending...
        let delEp = Link.deleteTransaction @'Byron sourceWallet t
        rDel <- request @ApiTxId ctx delEp Default Empty
        expectResponseCode @IO HTTP.status204 rDel

    it "TRANS_DELETE_02 -\
        \ Shelley: Cannot forget tx that is already in ledger" $ \ctx -> do
        (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx

        -- post transaction
        rTx <-
            postTx ctx
            (wSrc, Link.createTransaction @'Shelley, "cardano-wallet")
            wDest
            (1 :: Natural)
        let txid = getFromResponse #id rTx

        eventually "Transaction is accepted" $ do
            let ep = Link.listTransactions @'Shelley wSrc
            request @([ApiTransaction n]) ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        -- Try Forget transaction once it's no longer pending
        let ep = Link.deleteTransaction @'Shelley wSrc (ApiTxId txid)
        rDel <- request @ApiTxId ctx ep Default Empty
        expectResponseCode @IO HTTP.status403 rDel
        let err = errMsg403NoPendingAnymore (toUrlPiece (ApiTxId txid))
        expectErrorMessage err rDel

    it "BYRON_TRANS_DELETE_02 -\
        \ Byron: Cannot forget tx that is already in ledger" $ \ctx -> do
        w <- fixtureRandomWallet ctx

        -- Get TX id
        let listEp = Link.listTransactions @'Byron w
        (_, t:_) <- unsafeRequest @([ApiTransaction n]) ctx listEp Empty
        let txid = t ^. #id

        -- Try Forget transaction that is no longer pending
        let delEp = Link.deleteTransaction @'Byron w t
        rDel <- request @ApiTxId ctx delEp Default Empty
        expectResponseCode @IO HTTP.status403 rDel
        let err = errMsg403NoPendingAnymore (toUrlPiece (ApiTxId txid))
        expectErrorMessage err rDel

    describe "TRANS_DELETE_03 - checking no transaction id error for " $ do
        txDeleteNotExistsingTxIdTest emptyWallet "wallets"
        txDeleteNotExistsingTxIdTest emptyRandomWallet "byron-wallets"

    describe "TRANS_DELETE_06 -\
        \ Cannot forget tx that is performed from different wallet" $ do
        txDeleteFromDifferentWalletTest emptyWallet "wallets"
        txDeleteFromDifferentWalletTest emptyRandomWallet "byron-wallets"

    it "BYRON_TRANS_DELETE -\
        \ Cannot delete tx on Byron wallet using shelley ep" $ \ctx -> do
            w <- emptyRandomWallet ctx
            let wid = w ^. walletId
            let txid = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
            let endpoint = "v2/wallets/" <> wid <> "/transactions/" <> txid
            r <- request @ApiTxId @IO ctx ("DELETE", endpoint) Default Empty
            expectResponseCode HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_TRANS_ESTIMATE -\
        \ Cannot estimate tx on Byron wallet using shelley ep" $ \ctx -> do
            w <- emptyRandomWallet ctx
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
            let endpoint = "v2/wallets/" <> wid <> "/payment-fees"
            r <- request @ApiFee ctx ("POST", endpoint) Default payload
            expectResponseCode @IO HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet wid) r

    it "BYRON_TRANS_CREATE -\
        \ Cannot create tx on Byron wallet using shelley ep" $ \ctx -> do
            w <- emptyRandomWallet ctx
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

    it "BYRON_TX_LIST_02 -\
        \ Byron endpoint does not list Shelley wallet transactions" $ \ctx -> do
        w <- emptyWallet ctx
        let wid = w ^. walletId
        let ep = ("GET", "v2/byron-wallets/" <> wid <> "/transactions")
        r <- request @([ApiTransaction n]) ctx ep Default Empty
        verify r
            [ expectResponseCode @IO HTTP.status404
            , expectErrorMessage (errMsg404NoWallet wid)
            ]

    it "BYRON_TX_LIST_03 -\
        \ Shelley endpoint does not list Byron wallet transactions" $ \ctx -> do
        w <- emptyRandomWallet ctx
        let wid = w ^. walletId
        let ep = ("GET", "v2/wallets/" <> wid <> "/transactions")
        r <- request @([ApiTransaction n]) ctx ep Default Empty
        verify r
            [ expectResponseCode @IO HTTP.status404
            , expectErrorMessage (errMsg404NoWallet wid)
            ]
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

    txDeleteFromDifferentWalletTest
        :: (HasType (ApiT WalletId) wal)
        => (Context t -> IO wal)
        -> String
        -> SpecWith (Context t)
    txDeleteFromDifferentWalletTest eWallet resource =
        it resource $ \ctx -> do
            -- post tx
            (wSrc, wDest) <- (,) <$> fixtureWallet ctx <*> emptyWallet ctx
            rMkTx <- postTx ctx
                (wSrc, Link.createTransaction @'Shelley, "cardano-wallet")
                wDest
                (1 :: Natural)

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
