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
    ( Iso8601Time (..) )
import Cardano.Wallet.Api.Types
    ( ApiFee, ApiTransaction, ApiWallet, getApiT )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress (..)
    , Direction (..)
    , EncodeAddress (..)
    , TxStatus (..)
    , encodeAddress
    )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Typed
    ( typed )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( toText )
import Network.Wai.Handler.Warp
    ( Port )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand
    , TxDescription (..)
    , amount
    , balanceAvailable
    , balanceTotal
    , cardanoWalletCLI
    , deleteWalletViaCLI
    , direction
    , emptyWallet
    , expectCliFieldBetween
    , expectCliFieldEqual
    , expectEventually'
    , expectValidJSON
    , faucetAmt
    , faucetUtxoAmt
    , feeEstimator
    , fixtureWallet
    , fixtureWalletWith
    , getWalletViaCLI
    , listAddresses
    , listTransactionsViaCLI
    , postTransactionFeeViaCLI
    , postTransactionViaCLI
    , status
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
    , falseWalletIds
    , kanjiWalletName
    , polishWalletName
    , wildcardsWalletName
    )
import Test.QuickCheck
    ( property, withMaxSuccess )
import Test.QuickCheck.Instances.Time
    ()

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

        expectEventually' ctx balanceAvailable amt wDest
        expectEventually' ctx balanceTotal amt wDest

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

        expectEventually' ctx balanceAvailable (2*amt) wDest
        expectEventually' ctx balanceTotal (2*amt) wDest

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
            expectEventually' ctx balanceAvailable amt wDest
            expectEventually' ctx balanceTotal amt wDest

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

        expectEventually' ctx balanceAvailable amt wDest
        expectEventually' ctx balanceTotal amt wDest

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
            let port = show $ ctx ^. typed @Port
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
        let port = T.pack $ show $ ctx ^. typed @Port
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
        let port = T.pack $ show $ ctx ^. typed @Port
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

    it "TRANS_LIST_01 - Listing transactions for an empty wallet" $ \ctx ->
        withMaxSuccess 10 $ property $ \mAfter mBefore -> do
            wallet <- emptyWallet ctx
            (Exit code, Stdout out, Stderr err) <-
                listTransactionsViaCLI @t ctx wallet
                    (Iso8601Time <$> mAfter)
                    (Iso8601Time <$> mBefore)
            err `shouldBe` "Ok.\n"
            out `shouldBe` "[]\n"
            code `shouldBe` ExitSuccess

  where
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
