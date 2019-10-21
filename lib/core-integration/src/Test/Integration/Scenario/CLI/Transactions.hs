{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Transactions
    ( spec
    ) where

import Prelude

import Cardano.CLI
    ( Port )
import Cardano.Wallet.Api.Types
    ( ApiFee
    , ApiTransaction
    , ApiTxId (..)
    , ApiWallet
    , getApiT
    , insertedAt
    , time
    )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress (..)
    , Direction (..)
    , EncodeAddress (..)
    , SortOrder (..)
    , TxStatus (..)
    , encodeAddress
    )
import Control.Monad
    ( forM_, join )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Product.Typed
    ( typed )
import Data.List.Extra
    ( enumerate )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( showT, toText )
import Data.Time.Clock
    ( UTCTime )
import Data.Time.Utils
    ( utcTimePred, utcTimeSucc )
import Numeric.Natural
    ( Natural )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldSatisfy )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand
    , TxDescription (..)
    , amount
    , balanceAvailable
    , balanceTotal
    , cardanoWalletCLI
    , deleteTransactionViaCLI
    , deleteWalletViaCLI
    , direction
    , emptyWallet
    , expectCliFieldBetween
    , expectCliFieldEqual
    , expectCliListItemFieldEqual
    , expectEventually'
    , expectValidJSON
    , faucetAmt
    , faucetUtxoAmt
    , feeEstimator
    , fixtureWallet
    , fixtureWalletWith
    , getWalletEp
    , getWalletViaCLI
    , listAddresses
    , listAllTransactions
    , listTransactionsViaCLI
    , postTransactionFeeViaCLI
    , postTransactionViaCLI
    , status
    , utcIso8601ToText
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , errMsg403Fee
    , errMsg403InputsDepleted
    , errMsg403NotEnoughMoney
    , errMsg403UTxO
    , errMsg403WrongPass
    , errMsg404NoWallet
    , falseWalletIds
    , kanjiWalletName
    , polishWalletName
    , wildcardsWalletName
    )
import Web.HttpApiData
    ( ToHttpApiData (..) )

import qualified Data.Text as T

spec
    :: forall t. (EncodeAddress t, DecodeAddress t, KnownCommand t)
    => SpecWith (Context t)
spec = do
    it "TRANS_CREATE_01 - Can create transaction via CLI" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest
        let addrStr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addr ^. #id)
        let amt = 14
        let (feeMin, feeMax) = ctx ^. feeEstimator $ TxDescription
                { nInputs = 1
                , nOutputs = 1
                }
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addrStr
                ]

        -- post transaction
        (c, out, err) <- postTransactionViaCLI @t ctx "cardano-wallet" args
        err `shouldBe` "Please enter your passphrase: **************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction t)) out
        verify txJson
            [ expectCliFieldBetween amount (feeMin + amt, feeMax + amt)
            , expectCliFieldEqual direction Outgoing
            , expectCliFieldEqual status Pending
            ]
        c `shouldBe` ExitSuccess

        -- verify balance on src wallet
        Stdout gOutSrc <- getWalletViaCLI @t ctx (T.unpack (wSrc ^. walletId))
        gJson <- expectValidJSON (Proxy @ApiWallet) gOutSrc
        verify gJson
            [ expectCliFieldBetween balanceTotal
                ( faucetAmt - feeMax - amt
                , faucetAmt - feeMin - amt
                )
            , expectCliFieldEqual balanceAvailable (faucetAmt - faucetUtxoAmt)
            ]

        expectEventually' ctx getWalletEp balanceAvailable amt wDest
        expectEventually' ctx getWalletEp balanceTotal amt wDest

        -- verify balance on dest wallet
        Stdout gOutDest <- getWalletViaCLI @t ctx (T.unpack (wDest ^. walletId))
        destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
        verify destJson
            [ expectCliFieldEqual balanceAvailable amt
            , expectCliFieldEqual balanceTotal amt
            ]

    it "TRANS_CREATE_02 - Multiple Output Tx to single wallet via CLI" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr <- listAddresses ctx wDest
        let addr1 =
                encodeAddress (Proxy @t) (getApiT $ fst $ addr !! 1 ^. #id)
        let addr2 =
                encodeAddress (Proxy @t) (getApiT $ fst $ addr !! 2 ^. #id)
        let amt = 14
        let (feeMin, feeMax) = ctx ^. feeEstimator $ TxDescription
                { nInputs = 2
                , nOutputs = 2
                }
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addr1
                , "--payment", T.pack (show amt) <> "@" <> addr2
                ]

        -- post transaction
        (c, out, err) <- postTransactionViaCLI @t ctx "cardano-wallet" args
        err `shouldBe` "Please enter your passphrase: **************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction t)) out
        verify txJson
            [ expectCliFieldBetween amount (feeMin + (2*amt), feeMax + (2*amt))
            , expectCliFieldEqual direction Outgoing
            , expectCliFieldEqual status Pending
            ]
        c `shouldBe` ExitSuccess

        -- verify balance on src wallet
        Stdout gOutSrc <- getWalletViaCLI @t ctx (T.unpack (wSrc ^. walletId))
        gJson <- expectValidJSON (Proxy @ApiWallet) gOutSrc
        verify gJson
            [ expectCliFieldBetween balanceTotal
                ( faucetAmt - feeMax - (2*amt)
                , faucetAmt - feeMin - (2*amt)
                )
            , expectCliFieldEqual balanceAvailable (faucetAmt - 2*faucetUtxoAmt)
            ]

        expectEventually' ctx getWalletEp balanceAvailable (2*amt) wDest
        expectEventually' ctx getWalletEp balanceTotal (2*amt) wDest

        -- verify balance on dest wallet
        Stdout gOutDest <- getWalletViaCLI @t ctx (T.unpack (wDest ^. walletId))
        destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
        verify destJson
            [ expectCliFieldEqual balanceAvailable (2*amt)
            , expectCliFieldEqual balanceTotal (2*amt)
            ]

    it "TRANS_CREATE_02 - Multiple Output Tx to different wallets via CLI" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest1 <- emptyWallet ctx
        wDest2 <- emptyWallet ctx
        addr1:_ <- listAddresses ctx wDest1
        addr2:_ <- listAddresses ctx wDest2
        let addr1' =
                encodeAddress (Proxy @t) (getApiT $ fst $ addr1 ^. #id)
        let addr2' =
                encodeAddress (Proxy @t) (getApiT $ fst $ addr2 ^. #id)
        let amt = 14
        let (feeMin, feeMax) = ctx ^. feeEstimator $ TxDescription
                { nInputs = 2
                , nOutputs = 2
                }
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addr1'
                , "--payment", T.pack (show amt) <> "@" <> addr2'
                ]

        -- post transaction
        (c, out, err) <- postTransactionViaCLI @t ctx "cardano-wallet" args
        err `shouldBe` "Please enter your passphrase: **************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction t)) out
        verify txJson
            [ expectCliFieldBetween amount (feeMin + (2*amt), feeMax + (2*amt))
            , expectCliFieldEqual direction Outgoing
            , expectCliFieldEqual status Pending
            ]
        c `shouldBe` ExitSuccess

        -- verify balance on src wallet
        Stdout gOutSrc <- getWalletViaCLI @t ctx (T.unpack (wSrc ^. walletId))
        gJson <- expectValidJSON (Proxy @ApiWallet) gOutSrc
        verify gJson
            [ expectCliFieldBetween balanceTotal
                ( faucetAmt - feeMax - (2*amt)
                , faucetAmt - feeMin - (2*amt)
                )
            , expectCliFieldEqual balanceAvailable (faucetAmt - 2*faucetUtxoAmt)
            ]

        forM_ [wDest1, wDest2] $ \wDest -> do
            expectEventually' ctx getWalletEp balanceAvailable amt wDest
            expectEventually' ctx getWalletEp balanceTotal amt wDest

            -- verify balance on dest wallets
            Stdout gOutDest <- getWalletViaCLI @t ctx (T.unpack (wDest ^. walletId))
            destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
            verify destJson
                [ expectCliFieldEqual balanceAvailable amt
                , expectCliFieldEqual balanceTotal amt
                ]

    it "TRANS_CREATE_02 - Multiple Output Txs don't work on single UTxO" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [2_124_333]
        wDest <- emptyWallet ctx
        addrs <- listAddresses ctx wDest

        let addr1 =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs !! 1 ^. #id)
        let addr2 =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs !! 2 ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "12333@" <> addr1
                , "--payment", "4666@" <> addr2
                ]

        (c, out, err) <- postTransactionViaCLI @t ctx "cardano-wallet" args
        (T.unpack err) `shouldContain` errMsg403UTxO
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_03 - 0 balance after transaction" $ \ctx -> do
        let (feeMin, _) = ctx ^. feeEstimator $ TxDescription 1 1
        let amt = 1
        wSrc <- fixtureWalletWith ctx [feeMin+amt]
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses ctx wDest
        let addr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", toText amt <> "@" <> addr
                ]

        (c, out, err) <- postTransactionViaCLI @t ctx "Secure Passphrase" args
        err `shouldBe` "Please enter your passphrase: *****************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction t)) out
        verify txJson
            [ expectCliFieldEqual amount (feeMin+amt)
            , expectCliFieldEqual direction Outgoing
            , expectCliFieldEqual status Pending
            ]
        c `shouldBe` ExitSuccess

        Stdout gOutSrc <- getWalletViaCLI @t ctx (T.unpack (wSrc ^. walletId))
        gJson <- expectValidJSON (Proxy @ApiWallet) gOutSrc
        verify gJson
            [ expectCliFieldEqual balanceTotal 0
            , expectCliFieldEqual balanceAvailable 0
            ]

        expectEventually' ctx getWalletEp balanceAvailable amt wDest
        expectEventually' ctx getWalletEp balanceTotal amt wDest

        Stdout gOutDest <- getWalletViaCLI @t ctx (T.unpack (wDest ^. walletId))
        destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
        verify destJson
            [ expectCliFieldEqual balanceAvailable amt
            , expectCliFieldEqual balanceTotal amt
            ]

    it "TRANS_CREATE_04 - Error shown when ErrInputsDepleted encountered" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [12_000_000, 20_000_000, 17_000_000]
        wDest <- emptyWallet ctx
        addrs <- listAddresses ctx wDest

        let addr1 = encodeAddress (Proxy @t) (getApiT $ fst $ addrs !! 1 ^. #id)
        let addr2 = encodeAddress (Proxy @t) (getApiT $ fst $ addrs !! 2 ^. #id)
        let addr3 = encodeAddress (Proxy @t) (getApiT $ fst $ addrs !! 3 ^. #id)

        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "40000000@" <> addr1
                , "--payment", "22@" <> addr2
                , "--payment", "22@" <> addr3
                ]

        (c, out, err) <- postTransactionViaCLI @t ctx "Secure Passphrase" args
        (T.unpack err) `shouldContain` errMsg403InputsDepleted
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_04 - Can't cover fee" $ \ctx -> do
        let (feeMin, _) = ctx ^. feeEstimator $ TxDescription 1 1
        wSrc <- fixtureWalletWith ctx [feeMin `div` 2]
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses ctx wDest
        let addr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "1@" <> addr
                ]

        (c, out, err) <- postTransactionViaCLI @t ctx "Secure Passphrase" args
        (T.unpack err) `shouldContain` errMsg403Fee
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_04 - Not enough money" $ \ctx -> do
        let (feeMin, _) = ctx ^. feeEstimator $ TxDescription 1 1
        wSrc <- fixtureWalletWith ctx [feeMin]
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses ctx wDest
        let addr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "1000000@" <> addr
                ]

        (c, out, err) <- postTransactionViaCLI @t ctx "Secure Passphrase" args
        (T.unpack err) `shouldContain`
            errMsg403NotEnoughMoney (fromIntegral feeMin) 1_000_000
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_04 - Wrong password" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses ctx wDest
        let addr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "14@" <> addr
                ]

        (c, out, err) <- postTransactionViaCLI @t ctx "This password is wrong" args
        (T.unpack err) `shouldContain` errMsg403WrongPass
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    describe "TRANS_CREATE_05 - Invalid addresses" $ do
        forM_ matrixInvalidAddrs $ \(title, addr, errMsg) -> it title $ \ctx -> do
            wSrc <- emptyWallet ctx
            let args = T.unpack <$>
                    [ wSrc ^. walletId
                    , "--payment", "12@" <> (T.pack addr)
                    ]

            (c, out, err) <- postTransactionViaCLI @t ctx "Secure Passphrase" args
            (T.unpack err) `shouldContain` errMsg
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_CREATE_06 - Invalid amount" $ do
        forM_ matrixInvalidAmt $ \(title, amt, errMsg) -> it title $ \ctx -> do
            wSrc <- emptyWallet ctx
            wDest <- emptyWallet ctx
            addrs:_ <- listAddresses ctx wDest
            let addr =
                    encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
            let args = T.unpack <$>
                    [ wSrc ^. walletId
                    , "--payment", amt <> "@" <> addr
                    ]

            (c, out, err) <- postTransactionViaCLI @t ctx "cardano-wallet" args
            (T.unpack err) `shouldContain` errMsg
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_CREATE_07 - False wallet ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            wDest <- emptyWallet ctx
            addrs:_ <- listAddresses ctx wDest
            let port = show $ ctx ^. typed @(Port "wallet")
            let addr = encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
            let args =
                    [ "transaction", "create", "--port", port
                    , walId, "--payment", "12@" ++  (T.unpack addr)
                    ]
            -- make sure CLI returns error before asking for passphrase
            (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI @t args
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1
            if (title == "40 chars hex") then
                err `shouldContain` "I couldn't find a wallet with \
                    \the given id: 1111111111111111111111111111111111111111"
            else
                err `shouldContain` "wallet id should be an \
                    \hex-encoded string of 40 characters"

    it "TRANS_CREATE_07 - 'almost' valid walletId" $ \ctx -> do
        wSrc <- emptyWallet ctx
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses ctx wDest
        let port = T.pack $ show $ ctx ^. typed @(Port "wallet")
        let addr = encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ "transaction", "create", "--port", port
                , T.append (wSrc ^. walletId) "0", "--payment", "11@" <> addr
                ]
        -- make sure CLI returns error before asking for passphrase
        (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI @t args
        err `shouldContain` "wallet id should be an hex-encoded\
            \ string of 40 characters"
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_07 - Deleted wallet" $ \ctx -> do
        wSrc <- emptyWallet ctx
        Exit ex <- deleteWalletViaCLI @t ctx (T.unpack ( wSrc ^. walletId ))
        ex `shouldBe` ExitSuccess

        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses ctx wDest
        let addr = encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
        let port = T.pack $ show $ ctx ^. typed @(Port "wallet")
        let args = T.unpack <$>
                [ "transaction", "create", "--port", port
                , wSrc ^. walletId, "--payment", "11@" <> addr
                ]
        -- make sure CLI returns error before asking for passphrase
        (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI @t args
        err `shouldContain` "I couldn't find a wallet with \
            \the given id: " ++ T.unpack ( wSrc ^. walletId )
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_ESTIMATE_01 - Can estimate fee of transaction via CLI" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest
        let addrStr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addr ^. #id)
        let amt = 14
        let (feeMin, feeMax) = ctx ^. feeEstimator $ TxDescription
                { nInputs = 1
                , nOutputs = 1
                }
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addrStr
                ]
        (c, out, err) <- postTransactionFeeViaCLI @t ctx args
        err `shouldBe` "Ok.\n"
        txJson <- expectValidJSON (Proxy @ApiFee) out
        verify txJson
            [ expectCliFieldBetween amount (feeMin - amt, feeMax + amt)
            ]
        c `shouldBe` ExitSuccess

    it "TRANS_ESTIMATE_02 - Multiple Output Tx fees estimate to single wallet via CLI" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr <- listAddresses ctx wDest
        let addr1 =
                encodeAddress (Proxy @t) (getApiT $ fst $ addr !! 1 ^. #id)
        let addr2 =
                encodeAddress (Proxy @t) (getApiT $ fst $ addr !! 2 ^. #id)
        let amt = 14
        let (feeMin, feeMax) = ctx ^. feeEstimator $ TxDescription
                { nInputs = 2
                , nOutputs = 2
                }
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addr1
                , "--payment", T.pack (show amt) <> "@" <> addr2
                ]
        (c, out, err) <- postTransactionFeeViaCLI @t ctx args
        err `shouldBe` "Ok.\n"
        txJson <- expectValidJSON (Proxy @ApiFee) out
        verify txJson
            [ expectCliFieldBetween amount (feeMin - (2*amt), feeMax + (2*amt))
            ]
        c `shouldBe` ExitSuccess


    it "TRANS_ESTIMATE_03 - Multiple Output Tx fees estimation to different wallets via CLI" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest1 <- emptyWallet ctx
        wDest2 <- emptyWallet ctx
        addr1:_ <- listAddresses ctx wDest1
        addr2:_ <- listAddresses ctx wDest2
        let addr1' =
                encodeAddress (Proxy @t) (getApiT $ fst $ addr1 ^. #id)
        let addr2' =
                encodeAddress (Proxy @t) (getApiT $ fst $ addr2 ^. #id)
        let amt = 14
        let (feeMin, feeMax) = ctx ^. feeEstimator $ TxDescription
                { nInputs = 2
                , nOutputs = 2
                }
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addr1'
                , "--payment", T.pack (show amt) <> "@" <> addr2'
                ]
        (c, out, err) <- postTransactionFeeViaCLI @t ctx args
        err `shouldBe` "Ok.\n"
        txJson <- expectValidJSON (Proxy @ApiFee) out
        verify txJson
            [ expectCliFieldBetween amount (feeMin - (2*amt), feeMax + (2*amt))
            ]
        c `shouldBe` ExitSuccess

    it "TRANS_ESTIMATE_04 - Multiple Output Txs fees estimation doesn't work on single UTxO" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [2_124_333]
        wDest <- emptyWallet ctx
        addrs <- listAddresses ctx wDest

        let addr1 =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs !! 1 ^. #id)
        let addr2 =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs !! 2 ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "12333@" <> addr1
                , "--payment", "4666@" <> addr2
                ]
        (c, out, err) <- postTransactionFeeViaCLI @t ctx args
        (T.unpack err) `shouldContain` errMsg403UTxO
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_ESTIMATE_05 - Error shown when ErrInputsDepleted encountered" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [12_000_000, 20_000_000, 17_000_000]
        wDest <- emptyWallet ctx
        addrs <- listAddresses ctx wDest

        let addr1 = encodeAddress (Proxy @t) (getApiT $ fst $ addrs !! 1 ^. #id)
        let addr2 = encodeAddress (Proxy @t) (getApiT $ fst $ addrs !! 2 ^. #id)
        let addr3 = encodeAddress (Proxy @t) (getApiT $ fst $ addrs !! 3 ^. #id)

        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "40000000@" <> addr1
                , "--payment", "22@" <> addr2
                , "--payment", "22@" <> addr3
                ]

        (c, out, err) <- postTransactionFeeViaCLI @t ctx args
        (T.unpack err) `shouldContain` errMsg403InputsDepleted
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_ESTIMATE_06 - we give fee estimation when we can't cover fee" $ \ctx -> do
        let (feeMin, _) = ctx ^. feeEstimator $ TxDescription 1 1
        wSrc <- fixtureWalletWith ctx [feeMin `div` 2]
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses ctx wDest
        let addr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "1@" <> addr
                ]

        (c, _, err) <- postTransactionFeeViaCLI @t ctx args
        err `shouldBe` "Ok.\n"
        c `shouldBe` ExitSuccess

    it "TRANS_ESTIMATE_07 - Not enough money" $ \ctx -> do
        let (feeMin, _) = ctx ^. feeEstimator $ TxDescription 1 1
        wSrc <- fixtureWalletWith ctx [feeMin]
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses ctx wDest
        let addr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "1000000@" <> addr
                ]

        (c, out, err) <- postTransactionFeeViaCLI @t ctx args
        (T.unpack err) `shouldContain`
            errMsg403NotEnoughMoney (fromIntegral feeMin) 1_000_000
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    describe "TRANS_ESTIMATE_08 - Invalid addresses" $ do
        forM_ matrixInvalidAddrs $ \(title, addr, errMsg) -> it title $ \ctx -> do
            wSrc <- emptyWallet ctx
            let args = T.unpack <$>
                    [ wSrc ^. walletId
                    , "--payment", "12@" <> (T.pack addr)
                    ]

            (c, out, err) <- postTransactionFeeViaCLI @t ctx args
            (T.unpack err) `shouldContain` errMsg
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_ESTIMATE_09 - Invalid amount" $ do
        forM_ matrixInvalidAmt $ \(title, amt, errMsg) -> it title $ \ctx -> do
            wSrc <- emptyWallet ctx
            wDest <- emptyWallet ctx
            addrs:_ <- listAddresses ctx wDest
            let addr =
                    encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
            let args = T.unpack <$>
                    [ wSrc ^. walletId
                    , "--payment", amt <> "@" <> addr
                    ]

            (c, out, err) <- postTransactionViaCLI @t ctx "cardano-wallet" args
            (T.unpack err) `shouldContain` errMsg
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_LIST_01 - Listing transactions for an empty wallet" $
        forM_ timeRangeMatrix $ \(mStart, mEnd) -> do
            forM_ sortOrderMatrix $ \mOrder -> do
                let title = mempty
                      <> "listing transactions from "
                      <> show mStart
                      <> " to "
                      <> show mEnd
                      <> " in "
                      <> show mOrder
                      <> " order "
                it title $ \ctx -> do
                    wallet <- emptyWallet ctx
                    (Exit code, Stdout out, Stderr err) <-
                        listTransactionsViaCLI @t ctx $ join
                            [ [T.unpack $ wallet ^. walletId]
                            , maybe [] (\t -> ["--start", t]) mStart
                            , maybe [] (\t -> ["--end"  , t]) mEnd
                            , maybe [] (\o -> ["--order", showT o]) mOrder
                            ]
                    err `shouldBe` "Ok.\n"
                    out `shouldBe` "[]\n"
                    code `shouldBe` ExitSuccess

    it "TRANS_LIST_01 - Can list Incoming and Outgoing transactions" $ \ctx -> do
        -- Make tx from fixtureWallet
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest
        let addrStr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addr ^. #id)
        let amt = 14 :: Natural
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addrStr
                ]

        -- post transaction
        (c, _, _) <- postTransactionViaCLI @t ctx "cardano-wallet" args
        c `shouldBe` ExitSuccess
        expectEventually' ctx getWalletEp balanceAvailable amt wDest
        expectEventually' ctx getWalletEp balanceTotal amt wDest

        -- Verify Tx list contains Incoming and Outgoing
        (Exit code, Stdout out, Stderr err) <-
            listTransactionsViaCLI @t ctx [T.unpack $ wSrc ^. walletId]
        err `shouldBe` "Ok.\n"
        code `shouldBe` ExitSuccess
        outJson <- expectValidJSON (Proxy @([ApiTransaction t])) out
        verify outJson
            [ expectCliListItemFieldEqual 0 direction Outgoing
            , expectCliListItemFieldEqual 1 direction Incoming
            ]

    describe "TRANS_LIST_02 - Start time shouldn't be later than end time" $
        forM_ sortOrderMatrix $ \mOrder -> do
            let startTime = max validTime1 validTime2
            let endTime   = min validTime1 validTime2
            let title = mempty
                    <> "listing transactions from "
                    <> show startTime
                    <> " to "
                    <> show endTime
                    <> " in "
                    <> show mOrder
                    <> " order "
            it title $ \ctx -> do
                wid <- emptyWallet' ctx
                (Exit code, Stdout out, Stderr err) <-
                    listTransactionsViaCLI @t ctx $ join
                        [ [ wid ]
                        , [ "--start", startTime ]
                        , [ "--end"  , endTime ]
                        , maybe [] (\o -> ["--order", showT o]) mOrder
                        ]
                err `shouldBe` mconcat
                    [ "The specified start time '"
                    , startTime
                    , "' is later than the specified end time '"
                    , endTime
                    , "'.\n"
                    ]
                out `shouldBe` mempty
                code `shouldBe` ExitFailure 1

    it "TRANS_LIST_03 - Can order results" $ \ctx -> do
        let a1 = sum $ replicate 10 1
        let a2 = sum $ replicate 10 2
        w <- fixtureWalletWith ctx $ mconcat
                [ replicate 10 1
                , replicate 10 2
                ]
        let orderings =
                [ ( mempty
                  , [ expectCliListItemFieldEqual 0 amount a2
                    , expectCliListItemFieldEqual 1 amount a1
                    ]
                  )
                , ( [ "--order", "ascending" ]
                  , [ expectCliListItemFieldEqual 0 amount a1
                    , expectCliListItemFieldEqual 1 amount a2
                    ]
                  )
                , ( [ "--order", "descending" ]
                  , [ expectCliListItemFieldEqual 0 amount a2
                    , expectCliListItemFieldEqual 1 amount a1
                    ]
                  )
                ]

        forM_ orderings $ \(order, expects) -> do
            let args = T.unpack <$> w ^. walletId : order
            (Exit code, Stdout out, Stderr err) <-
                listTransactionsViaCLI @t ctx args
            err `shouldBe` "Ok.\n"
            code `shouldBe` ExitSuccess
            outJson <- expectValidJSON (Proxy @([ApiTransaction t])) out
            length outJson `shouldBe` 2
            verify outJson expects

    describe "TRANS_LIST_02,03 - Faulty start, end, order values" $ do
        let orderErr = "Please specify one of the following values:\
            \ ascending, descending."
        let startEndErr = "Expecting ISO 8601 date-and-time format\
            \ (basic or extended), e.g. 2012-09-25T10:15:00Z."
        let queries =
                  [ ( [ "--start", "2009" ]
                    , startEndErr
                    )
                  ,
                    ( [ "--start", "2012-09-25T10:15:00Z", "--end", "2016-11-21" ]
                    , startEndErr
                    )
                  ,
                    ( [ "--start", "2012-09-25", "--end", "2016-11-21T10:15:00Z" ]
                    , startEndErr
                    )
                  ,
                    ( [ "--end", "2012-09-25T10:15:00Z", "--start", "2016-11-21" ]
                    , startEndErr
                    )
                  ,
                    ( [ "--order", "scending" ]
                    , orderErr
                    )
                  ,
                    ( [ "--start", "2012-09-25T10:15:00Z", "--order", "asc" ]
                    , orderErr
                    )
                  ]
        forM_ queries $ \(q, errorMess) -> it (unwords q) $ \ctx -> do
            wid <- emptyWallet' ctx
            let args = wid : q
            (Exit code, Stdout out, Stderr err) <-
                listTransactionsViaCLI @t ctx args
            out `shouldBe` mempty
            code `shouldBe` ExitFailure 1
            err `shouldContain` errorMess

    it "TRANS_LIST_04 - 'almost' valid walletId" $ \ctx -> do
        wid <- emptyWallet' ctx
        let invalidWid = wid ++ "0"
        (Exit code, Stdout out, Stderr err) <-
            listTransactionsViaCLI @t ctx [invalidWid]

        err `shouldContain` "wallet id should be an hex-encoded\
            \ string of 40 characters"
        code `shouldBe` ExitFailure 1
        out `shouldBe` mempty

    it "TRANS_LIST_04 - Deleted wallet" $ \ctx -> do
        wid <- emptyWallet' ctx
        Exit d <- deleteWalletViaCLI @t ctx wid
        d `shouldBe` ExitSuccess

        (Exit c, Stdout o, Stderr e) <- listTransactionsViaCLI @t ctx [wid]
        e `shouldContain` errMsg404NoWallet (T.pack wid)
        o `shouldBe` mempty
        c `shouldBe` ExitFailure 1

    describe "TRANS_LIST_04 - False wallet ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            (Exit c, Stdout o, Stderr e) <- listTransactionsViaCLI @t ctx [walId]
            o `shouldBe` ""
            c `shouldBe` ExitFailure 1
            if (title == "40 chars hex") then
                e `shouldContain`
                    errMsg404NoWallet "1111111111111111111111111111111111111111"
            else
                e `shouldContain`
                    "wallet id should be an hex-encoded string of 40 characters"

    it "TRANS_LIST_RANGE_01 - \
       \Transaction at time t is SELECTED by small ranges that cover it" $
          \ctx -> do
              w <- fixtureWalletWith ctx [1]
              let walId = w ^. walletId
              t <- unsafeGetTransactionTime <$> listAllTransactions ctx w
              let (te, tl) = (utcTimePred t, utcTimeSucc t)
              let query t1 t2 =
                        [ "--start", utcIso8601ToText t1
                        , "--end", utcIso8601ToText t2
                        ]
              Stdout o1  <- listTransactionsViaCLI @t ctx
                    ( T.unpack <$> walId : (query t t) )
              Stdout o2 <- listTransactionsViaCLI @t ctx
                    ( T.unpack <$> walId : (query te t) )
              Stdout o3 <- listTransactionsViaCLI @t ctx
                    ( T.unpack <$> walId : (query t tl) )
              Stdout o4 <- listTransactionsViaCLI @t ctx
                    ( T.unpack <$> walId : (query te tl) )
              oJson1 <- expectValidJSON (Proxy @([ApiTransaction t])) o1
              oJson2 <- expectValidJSON (Proxy @([ApiTransaction t])) o2
              oJson3 <- expectValidJSON (Proxy @([ApiTransaction t])) o3
              oJson4 <- expectValidJSON (Proxy @([ApiTransaction t])) o4
              length <$> [oJson1, oJson2, oJson3, oJson4] `shouldSatisfy` all (== 1)

    it "TRANS_LIST_RANGE_02 - \
       \Transaction at time t is NOT selected by range [t + 𝛿t, ...)" $
          \ctx -> do
              w <- fixtureWalletWith ctx [1]
              let walId = w ^. walletId
              t <- unsafeGetTransactionTime <$> listAllTransactions ctx w
              let tl = utcIso8601ToText $ utcTimeSucc t
              Stdout o1  <- listTransactionsViaCLI @t ctx
                    ( T.unpack <$> [walId, "--start", tl] )
              Stdout o2 <- listTransactionsViaCLI @t ctx
                    ( T.unpack <$> [walId, "--start", tl, "--end", tl] )
              oJson1 <- expectValidJSON (Proxy @([ApiTransaction t])) o1
              oJson2 <- expectValidJSON (Proxy @([ApiTransaction t])) o2
              length <$> [oJson1, oJson2] `shouldSatisfy` all (== 0)

    it "TRANS_LIST_RANGE_03 - \
       \Transaction at time t is NOT selected by range (..., t - 𝛿t]" $
          \ctx -> do
              w <- fixtureWalletWith ctx [1]
              let walId = w ^. walletId
              t <- unsafeGetTransactionTime <$> listAllTransactions ctx w
              let te = utcIso8601ToText $ utcTimePred t
              Stdout o1  <- listTransactionsViaCLI @t ctx
                      ( T.unpack <$> [walId, "--end", te] )
              Stdout o2 <- listTransactionsViaCLI @t ctx
                      ( T.unpack <$> [walId, "--start", te, "--end", te] )
              oJson1 <- expectValidJSON (Proxy @([ApiTransaction t])) o1
              oJson2 <- expectValidJSON (Proxy @([ApiTransaction t])) o2
              length <$> [oJson1, oJson2] `shouldSatisfy` all (== 0)

    it "TRANS_DELETE_01 - Can forget pending transaction via CLI" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest
        let addrStr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addr ^. #id)
        let amt = 1
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addrStr
                ]

        -- post transaction
        (c, out, err) <- postTransactionViaCLI @t ctx "cardano-wallet" args
        err `shouldBe` "Please enter your passphrase: **************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction t)) out
        verify txJson
            [ expectCliFieldEqual direction Outgoing
            , expectCliFieldEqual status Pending
            ]
        c `shouldBe` ExitSuccess

        let txId' = txJson ^. #id
        let txId = toUrlPiece (ApiTxId txId')

        -- verify balance on src wallet
        Stdout gOutSrc <- getWalletViaCLI @t ctx (T.unpack (wSrc ^. walletId))
        gJson <- expectValidJSON (Proxy @ApiWallet) gOutSrc
        verify gJson
            [ expectCliFieldEqual balanceAvailable (faucetAmt - faucetUtxoAmt)
            ]

        -- forget transaction
        let wid = T.unpack $ wSrc ^. walletId
        Exit c1 <- deleteTransactionViaCLI @t ctx wid (T.unpack txId)
        c1 `shouldBe` ExitSuccess

        -- verify again balance on src wallet
        Stdout gOutSrc1 <- getWalletViaCLI @t ctx (T.unpack (wSrc ^. walletId))
        gJson1 <- expectValidJSON (Proxy @ApiWallet) gOutSrc1
        verify gJson1
            [ expectCliFieldEqual balanceAvailable faucetAmt
            ]

        expectEventually' ctx getWalletEp balanceAvailable amt wDest
        expectEventually' ctx getWalletEp balanceTotal amt wDest

        -- verify balance on dest wallet
        Stdout gOutDest <- getWalletViaCLI @t ctx (T.unpack (wDest ^. walletId))
        destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
        verify destJson
            [ expectCliFieldEqual balanceAvailable amt
            , expectCliFieldEqual balanceTotal amt
            ]
  where
      unsafeGetTransactionTime
          :: [ApiTransaction t]
          -> UTCTime
      unsafeGetTransactionTime txs =
          case fmap time . insertedAt <$> txs of
              (Just t):_ -> t
              _ -> error "Expected at least one transaction with a time."

      emptyWallet' :: Context t -> IO String
      emptyWallet' = fmap (T.unpack . view walletId) . emptyWallet

      sortOrderMatrix :: [Maybe SortOrder]
      sortOrderMatrix = Nothing : fmap pure enumerate

      validTime1 = "2001-01-01T01:01:01Z"
      validTime2 = "2009-09-09T09:09:09Z"

      timeRangeMatrix :: [(Maybe String, Maybe String)]
      timeRangeMatrix =
          [ (Nothing, Nothing)
          , (Just validTime1, Nothing)
          , (Nothing, Just validTime2)
          , (Just validTime1, Just validTime2)
          ]

      longAddr = replicate 10000 '1'
      encodeErr = "Unable to decode Address:"
      parseErr = "Parse error. Expecting format \"<amount>@<address>\""
      matrixInvalidAddrs =
          [ ( "long hex", longAddr, encodeErr )
          , ( "short hex", "1", encodeErr )
          , ( "-1000", "-1000", encodeErr )
          , ( "q", "q", encodeErr )
          , ( "empty", "", encodeErr )
          , ( "wildcards", T.unpack wildcardsWalletName, parseErr )
          , ( "arabic", T.unpack arabicWalletName, encodeErr )
          , ( "kanji", T.unpack kanjiWalletName, encodeErr )
          , ( "polish", T.unpack polishWalletName, encodeErr )
          , ( "[]", "[]", encodeErr )
          , ( "no address", "", encodeErr )
          , ( "address is space", " ", encodeErr )
          ]
      errNum = "Expecting natural number"
      matrixInvalidAmt =
          [ ("1.5", "1.5", errNum)
          , ("-1000", "-1000", errNum)
          , ("[]", "[]", errNum)
          , ("string with diacritics", polishWalletName, errNum)
          , ("string with wildcards", wildcardsWalletName, parseErr)
          , ("no amount", "", errNum)
          ]
