{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.CLI.Shelley.Transactions
    ( spec
    ) where

import Prelude

import Cardano.CLI
    ( Port )
import Cardano.Wallet.Api.Types
    ( ApiFee
    , ApiTransaction
    , ApiWallet
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , getApiT
    )
import Cardano.Wallet.Primitive.Types
    ( Direction (..), SortOrder (..), TxStatus (..) )
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
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( showT, toText )
import Data.Time.Utils
    ( utcTimePred, utcTimeSucc )
import Numeric.Natural
    ( Natural )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand
    , TxDescription (..)
    , between
    , cardanoWalletCLI
    , deleteTransactionViaCLI
    , deleteWalletViaCLI
    , emptyRandomWallet
    , emptyWallet
    , eventually
    , expectCliField
    , expectCliListField
    , expectValidJSON
    , faucetAmt
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWith
    , getTransactionViaCLI
    , getTxId
    , getWalletViaCLI
    , listAddresses
    , listAllTransactions
    , listTransactionsViaCLI
    , postTransactionFeeViaCLI
    , postTransactionViaCLI
    , unsafeGetTransactionTime
    , utcIso8601ToText
    , verify
    , walletId
    , (.>=)
    )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , errMsg403Fee
    , errMsg403NoPendingAnymore
    , errMsg403NotEnoughMoney
    , errMsg403WrongPass
    , errMsg404CannotFindTx
    , errMsg404NoWallet
    , falseWalletIds
    , kanjiWalletName
    , polishWalletName
    , wildcardsWalletName
    )

import qualified Data.Text as T

spec :: forall n t.
    ( KnownCommand t
    , DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    ) => SpecWith (Context t)
spec = do

    it "TRANS_CREATE_01 - Can create transaction via CLI" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
                { nInputs = 1
                , nOutputs = 1
                , nChanges = 1
                }
        let amt = 14
        txJson <- postTxViaCLI ctx wSrc wDest amt
        verify txJson
            [ expectCliField (#amount . #getQuantity)
                (between (feeMin + amt, feeMax + amt))
            , expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectCliField (#status . #getApiT) (`shouldBe` Pending)
            ]

        -- verify balance on src wallet
        Stdout gOutSrc <- getWalletViaCLI @t ctx (T.unpack (wSrc ^. walletId))
        gJson <- expectValidJSON (Proxy @ApiWallet) gOutSrc
        verify gJson
            [ expectCliField
                (#balance . #getApiT . #total)
                (.>= Quantity (faucetAmt - feeMax - amt))
            ]

        eventually "balance on dest wallet is OK" $ do
            Stdout gOutDest <- getWalletViaCLI @t ctx
                (T.unpack (wDest ^. walletId))
            destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
            verify destJson
                [ expectCliField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity amt)
                , expectCliField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity amt)
                ]

    it "TRANS_CREATE_02 - Multiple Output Tx to single wallet via CLI" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr <- listAddresses @n ctx wDest
        let addr1 = encodeAddress @n (getApiT $ fst $ addr !! 1 ^. #id)
        let addr2 = encodeAddress @n (getApiT $ fst $ addr !! 2 ^. #id)
        let amt = 14
        let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
                { nInputs = 2
                , nOutputs = 2
                , nChanges = 2
                }
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addr1
                , "--payment", T.pack (show amt) <> "@" <> addr2
                ]

        -- post transaction
        (c, out, err) <- postTransactionViaCLI @t ctx "cardano-wallet" args
        err `shouldBe` "Please enter your passphrase: **************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction n)) out
        verify txJson
            [ expectCliField
                (#amount . #getQuantity)
                (between (feeMin + (2*amt), feeMax + (2*amt)))
            , expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectCliField (#status . #getApiT) (`shouldBe` Pending)
            ]
        c `shouldBe` ExitSuccess

        -- verify balance on src wallet
        Stdout gOutSrc <- getWalletViaCLI @t ctx (T.unpack (wSrc ^. walletId))
        gJson <- expectValidJSON (Proxy @ApiWallet) gOutSrc
        verify gJson
            [ expectCliField
                (#balance . #getApiT . #total)
                (.>= Quantity (faucetAmt - feeMax - (2*amt)))
            ]

        eventually "balance on dest wallet is OK" $ do
            Stdout gOutDest <- getWalletViaCLI @t ctx
                (T.unpack (wDest ^. walletId))
            destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
            verify destJson
                [ expectCliField
                    (#balance . #getApiT . #available) (`shouldBe` Quantity (2*amt))
                , expectCliField
                    (#balance . #getApiT . #total) (`shouldBe` Quantity (2*amt))
                ]

    it "TRANS_CREATE_03 - 0 balance after transaction" $ \ctx -> do
        let (feeMin, _) = ctx ^. #_feeEstimator $ PaymentDescription 1 1 0
        let amt = 1
        wSrc <- fixtureWalletWith @n ctx [feeMin+amt]
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses @n ctx wDest
        let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", toText amt <> "@" <> addr
                ]

        (c, out, err) <- postTransactionViaCLI @t ctx
            (T.unpack fixturePassphrase) args
        err `shouldBe` "Please enter your passphrase: *****************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction n)) out
        verify txJson
            [ expectCliField (#amount . #getQuantity) (`shouldBe` feeMin+amt)
            , expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectCliField (#status . #getApiT) (`shouldBe` Pending)
            ]
        c `shouldBe` ExitSuccess

        Stdout gOutSrc <- getWalletViaCLI @t ctx (T.unpack (wSrc ^. walletId))
        gJson <- expectValidJSON (Proxy @ApiWallet) gOutSrc
        verify gJson
            [ expectCliField (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
            , expectCliField (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            ]

        eventually "Balance on dest wallet is OK" $ do
            Stdout gOutDest <- getWalletViaCLI @t ctx
                (T.unpack (wDest ^. walletId))
            destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
            verify destJson
                [ expectCliField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity amt)
                , expectCliField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity amt)
                ]

    it "TRANS_CREATE_04 - Can't cover fee" $ \ctx -> do
        let (feeMin, _) = ctx ^. #_feeEstimator $ PaymentDescription 1 1 1
        wSrc <- fixtureWalletWith @n ctx [feeMin `div` 2]
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses @n ctx wDest
        let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "1@" <> addr
                ]

        (c, out, err) <- postTransactionViaCLI @t ctx
            (T.unpack fixturePassphrase) args
        (T.unpack err) `shouldContain` errMsg403Fee
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_04 - Not enough money" $ \ctx -> do
        let (feeMin, _) = ctx ^. #_feeEstimator $ PaymentDescription 1 1 1
        wSrc <- fixtureWalletWith @n ctx [feeMin]
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses @n ctx wDest
        let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "1000000@" <> addr
                ]

        (c, out, err) <- postTransactionViaCLI @t ctx
            (T.unpack fixturePassphrase) args
        (T.unpack err) `shouldContain`
            errMsg403NotEnoughMoney (fromIntegral feeMin) 1_000_000
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_04 - Wrong password" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses @n ctx wDest
        let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
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

            (c, out, err) <- postTransactionViaCLI @t ctx
                (T.unpack fixturePassphrase) args
            (T.unpack err) `shouldContain` errMsg
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_CREATE_06 - Invalid amount" $ do
        forM_ matrixInvalidAmt $ \(title, amt, errMsg) -> it title $ \ctx -> do
            wSrc <- emptyWallet ctx
            wDest <- emptyWallet ctx
            addrs:_ <- listAddresses @n ctx wDest
            let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
            let args = T.unpack <$>
                    [ wSrc ^. walletId
                    , "--payment", amt <> "@" <> addr
                    ]

            (c, out, err) <- postTransactionViaCLI @t ctx
                (T.unpack fixturePassphrase) args
            (T.unpack err) `shouldContain` errMsg
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_CREATE_07 - False wallet ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            wDest <- emptyWallet ctx
            addrs:_ <- listAddresses @n ctx wDest
            let port = show $ ctx ^. typed @(Port "wallet")
            let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
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
                err `shouldContain` "wallet id should be a \
                    \hex-encoded string of 40 characters"

    it "TRANS_CREATE_07 - 'almost' valid walletId" $ \ctx -> do
        wSrc <- emptyWallet ctx
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses @n ctx wDest
        let port = T.pack $ show $ ctx ^. typed @(Port "wallet")
        let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ "transaction", "create", "--port", port
                , T.append (wSrc ^. walletId) "0", "--payment", "11@" <> addr
                ]
        -- make sure CLI returns error before asking for passphrase
        (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI @t args
        err `shouldContain` "wallet id should be a hex-encoded\
            \ string of 40 characters"
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_07 - Deleted wallet" $ \ctx -> do
        wSrc <- emptyWallet ctx
        Exit ex <- deleteWalletViaCLI @t ctx (T.unpack ( wSrc ^. walletId ))
        ex `shouldBe` ExitSuccess

        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses @n ctx wDest
        let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
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
        addr:_ <- listAddresses @n ctx wDest
        let addrStr = encodeAddress @n (getApiT $ fst $ addr ^. #id)
        let amt = 14
        let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
                { nInputs = 1
                , nOutputs = 1
                , nChanges = 1
                }
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addrStr
                ]
        (Exit c, Stdout out, Stderr err) <- postTransactionFeeViaCLI @t ctx args
        err `shouldBe` "Ok.\n"
        txJson <- expectValidJSON (Proxy @ApiFee) out
        verify txJson
            [ expectCliField (#estimatedMin . #getQuantity) $
                between (feeMin - amt, feeMax + amt)
            ]
        c `shouldBe` ExitSuccess

    it "TRANS_ESTIMATE_02 - Multiple Output Tx fees estimate to single wallet via CLI" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr <- listAddresses @n ctx wDest
        let addr1 = encodeAddress @n (getApiT $ fst $ addr !! 1 ^. #id)
        let addr2 = encodeAddress @n (getApiT $ fst $ addr !! 2 ^. #id)
        let amt = 14 :: Natural
        let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
                { nInputs = 2
                , nOutputs = 2
                , nChanges = 2
                }
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addr1
                , "--payment", T.pack (show amt) <> "@" <> addr2
                ]
        (Exit c, Stdout out, Stderr err) <- postTransactionFeeViaCLI @t ctx args
        err `shouldBe` "Ok.\n"
        txJson <- expectValidJSON (Proxy @ApiFee) out
        verify txJson
            [ expectCliField (#estimatedMin . #getQuantity) $
                between (feeMin, feeMax)
            ]
        c `shouldBe` ExitSuccess


    it "TRANS_ESTIMATE_03 - Multiple Output Tx fees estimation to different wallets via CLI" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest1 <- emptyWallet ctx
        wDest2 <- emptyWallet ctx
        addr1:_ <- listAddresses @n ctx wDest1
        addr2:_ <- listAddresses @n ctx wDest2
        let addr1' = encodeAddress @n (getApiT $ fst $ addr1 ^. #id)
        let addr2' = encodeAddress @n (getApiT $ fst $ addr2 ^. #id)
        let amt = 14 :: Natural
        let (feeMin, feeMax) = ctx ^. #_feeEstimator $ PaymentDescription
                { nInputs = 2
                , nOutputs = 2
                , nChanges = 2
                }
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addr1'
                , "--payment", T.pack (show amt) <> "@" <> addr2'
                ]
        (Exit c, Stdout out, Stderr err) <- postTransactionFeeViaCLI @t ctx args
        err `shouldBe` "Ok.\n"
        txJson <- expectValidJSON (Proxy @ApiFee) out
        verify txJson
            [ expectCliField (#estimatedMin . #getQuantity) $
                between (feeMin, feeMax)
            ]
        c `shouldBe` ExitSuccess

    it "TRANS_ESTIMATE_06 - we give fee estimation when we can't cover fee" $ \ctx -> do
        let (feeMin, _) = ctx ^. #_feeEstimator $ PaymentDescription 1 1 1
        wSrc <- fixtureWalletWith @n ctx [feeMin `div` 2]
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses @n ctx wDest
        let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "1@" <> addr
                ]

        (Exit c, Stderr err) <- postTransactionFeeViaCLI @t ctx args
        err `shouldBe` "Ok.\n"
        c `shouldBe` ExitSuccess

    it "TRANS_ESTIMATE_07 - Not enough money" $ \ctx -> do
        let (feeMin, _) = ctx ^. #_feeEstimator $ PaymentDescription 1 1 1
        wSrc <- fixtureWalletWith @n ctx [feeMin]
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses @n ctx wDest
        let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "1000000@" <> addr
                ]

        (Exit c, Stdout out, Stderr err) <- postTransactionFeeViaCLI @t ctx args
        err `shouldContain`
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

            (Exit c, Stdout out, Stderr err) <- postTransactionFeeViaCLI @t ctx args
            err `shouldContain` errMsg
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_ESTIMATE_09 - Invalid amount" $ do
        forM_ matrixInvalidAmt $ \(title, amt, errMsg) -> it title $ \ctx -> do
            wSrc <- emptyWallet ctx
            wDest <- emptyWallet ctx
            addrs:_ <- listAddresses @n ctx wDest
            let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
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
        addr:_ <- listAddresses @n ctx wDest
        let addrStr = encodeAddress @n (getApiT $ fst $ addr ^. #id)
        let amt = 14 :: Natural
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addrStr
                ]

        -- post transaction
        (c, _, _) <- postTransactionViaCLI @t ctx "cardano-wallet" args
        c `shouldBe` ExitSuccess
        eventually "Balance on wallet is as expected" $ do
            Stdout gOutDest <- getWalletViaCLI @t ctx
                (T.unpack (wDest ^. walletId))
            destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
            verify destJson
                [ expectCliField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity amt)
                , expectCliField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity amt)
                ]

        -- Verify Tx list contains Incoming and Outgoing
        (Exit code, Stdout out, Stderr err) <-
            listTransactionsViaCLI @t ctx [T.unpack $ wSrc ^. walletId]
        err `shouldBe` "Ok.\n"
        code `shouldBe` ExitSuccess
        outJson <- expectValidJSON (Proxy @([ApiTransaction n])) out
        verify outJson
            [ expectCliListField 0 (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectCliListField 1 (#direction . #getApiT) (`shouldBe` Incoming)
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
        let a1 = Quantity $ sum $ replicate 10 1
        let a2 = Quantity $ sum $ replicate 10 2
        w <- fixtureWalletWith @n ctx $ mconcat
                [ replicate 10 1
                , replicate 10 2
                ]
        let orderings =
                [ ( mempty
                  , [ expectCliListField 0 #amount (`shouldBe` a2)
                    , expectCliListField 1 #amount (`shouldBe` a1)
                    ]
                  )
                , ( [ "--order", "ascending" ]
                  , [ expectCliListField 0 #amount (`shouldBe` a1)
                    , expectCliListField 1 #amount (`shouldBe` a2)
                    ]
                  )
                , ( [ "--order", "descending" ]
                  , [ expectCliListField 0 #amount (`shouldBe` a2)
                    , expectCliListField 1 #amount (`shouldBe` a1)
                    ]
                  )
                ]

        forM_ orderings $ \(order, expects) -> do
            let args = T.unpack <$> w ^. walletId : order
            (Exit code, Stdout out, Stderr err) <-
                listTransactionsViaCLI @t ctx args
            err `shouldBe` "Ok.\n"
            code `shouldBe` ExitSuccess
            outJson <- expectValidJSON (Proxy @([ApiTransaction n])) out
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

        err `shouldContain` "wallet id should be a hex-encoded\
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
                    "wallet id should be a hex-encoded string of 40 characters"

    it "TRANS_LIST_RANGE_01 - \
       \Transaction at time t is SELECTED by small ranges that cover it" $
          \ctx -> do
              w <- fixtureWalletWith @n ctx [1]
              let walId = w ^. walletId
              t <- unsafeGetTransactionTime <$> listAllTransactions @n ctx w
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
              oJson1 <- expectValidJSON (Proxy @([ApiTransaction n])) o1
              oJson2 <- expectValidJSON (Proxy @([ApiTransaction n])) o2
              oJson3 <- expectValidJSON (Proxy @([ApiTransaction n])) o3
              oJson4 <- expectValidJSON (Proxy @([ApiTransaction n])) o4
              length <$> [oJson1, oJson2, oJson3, oJson4] `shouldSatisfy` all (== 1)

    it "TRANS_LIST_RANGE_02 - \
       \Transaction at time t is NOT selected by range [t + 𝛿t, ...)" $
          \ctx -> do
              w <- fixtureWalletWith @n ctx [1]
              let walId = w ^. walletId
              t <- unsafeGetTransactionTime <$> listAllTransactions @n ctx w
              let tl = utcIso8601ToText $ utcTimeSucc t
              Stdout o1  <- listTransactionsViaCLI @t ctx
                    ( T.unpack <$> [walId, "--start", tl] )
              Stdout o2 <- listTransactionsViaCLI @t ctx
                    ( T.unpack <$> [walId, "--start", tl, "--end", tl] )
              oJson1 <- expectValidJSON (Proxy @([ApiTransaction n])) o1
              oJson2 <- expectValidJSON (Proxy @([ApiTransaction n])) o2
              length <$> [oJson1, oJson2] `shouldSatisfy` all (== 0)

    it "TRANS_LIST_RANGE_03 - \
       \Transaction at time t is NOT selected by range (..., t - 𝛿t]" $
          \ctx -> do
              w <- fixtureWalletWith @n ctx [1]
              let walId = w ^. walletId
              t <- unsafeGetTransactionTime <$> listAllTransactions @n ctx w
              let te = utcIso8601ToText $ utcTimePred t
              Stdout o1  <- listTransactionsViaCLI @t ctx
                      ( T.unpack <$> [walId, "--end", te] )
              Stdout o2 <- listTransactionsViaCLI @t ctx
                      ( T.unpack <$> [walId, "--start", te, "--end", te] )
              oJson1 <- expectValidJSON (Proxy @([ApiTransaction n])) o1
              oJson2 <- expectValidJSON (Proxy @([ApiTransaction n])) o2
              length <$> [oJson1, oJson2] `shouldSatisfy` all (== 0)

    it "TRANS_GET_01 - Can get Incoming and Outgoing transaction" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses @n ctx wDest
        let addrStr = encodeAddress @n (getApiT $ fst $ addr ^. #id)
        let amt = 14 :: Natural
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addrStr
                ]
        (c, out, err) <- postTransactionViaCLI @t ctx "cardano-wallet" args
        err `shouldBe` "Please enter your passphrase: **************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction n)) out
        verify txJson
            [ expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectCliField (#status . #getApiT) (`shouldBe` Pending)
            ]
        c `shouldBe` ExitSuccess

        eventually "Balance on wallet is as expected" $ do
            Stdout gOutDest <- getWalletViaCLI @t ctx
                (T.unpack (wDest ^. walletId))
            destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
            verify destJson
                [ expectCliField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity amt)
                , expectCliField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity amt)
                ]

        eventually "Transactions are available and in ledger" $ do
            let wSrcId = T.unpack (wSrc ^. walletId)
            let txId =  getTxId txJson

            -- Verify Tx in source wallet is Outgoing and InLedger
            (Exit code1, Stdout out1, Stderr err1) <-
                getTransactionViaCLI @t ctx wSrcId txId
            err1 `shouldBe` "Ok.\n"
            code1 `shouldBe` ExitSuccess
            outJson1 <- expectValidJSON (Proxy @(ApiTransaction n)) out1
            verify outJson1
                [ expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectCliField (#status . #getApiT) (`shouldBe` InLedger)
                ]

            let wDestId = T.unpack (wDest ^. walletId)
            -- Verify Tx in destination wallet is Incoming and InLedger
            (Exit code2, Stdout out2, Stderr err2) <-
                getTransactionViaCLI @t ctx wDestId txId
            err2 `shouldBe` "Ok.\n"
            code2 `shouldBe` ExitSuccess
            outJson2 <- expectValidJSON (Proxy @(ApiTransaction n)) out2
            verify outJson2
                [ expectCliField (#direction . #getApiT) (`shouldBe` Incoming)
                , expectCliField (#status . #getApiT) (`shouldBe` InLedger)
                ]

    it "TRANS_GET_02 - Deleted wallet" $ \ctx -> do
        wid <- emptyWallet' ctx
        Exit d <- deleteWalletViaCLI @t ctx wid
        d `shouldBe` ExitSuccess
        let txId = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"

        (Exit c, Stdout o, Stderr e) <- getTransactionViaCLI @t ctx wid txId
        e `shouldContain` errMsg404NoWallet (T.pack wid)
        o `shouldBe` mempty
        c `shouldBe` ExitFailure 1

    it "TRANS_GET_03 - Using wrong transaction id" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses @n ctx wDest
        let addrStr = encodeAddress @n (getApiT $ fst $ addr ^. #id)
        let amt = 14 :: Natural
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addrStr
                ]
        (c1, o1, e1) <- postTransactionViaCLI @t ctx "cardano-wallet" args
        e1 `shouldBe` "Please enter your passphrase: **************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction n)) o1
        verify txJson
            [ expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectCliField (#status . #getApiT) (`shouldBe` Pending)
            ]
        c1 `shouldBe` ExitSuccess

        let wid = T.unpack (wSrc ^. walletId)
        let txId = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
        (Exit c2, Stdout o2, Stderr e2) <- getTransactionViaCLI @t ctx wid txId
        e2 `shouldContain` errMsg404CannotFindTx (T.pack txId)
        o2 `shouldBe` mempty
        c2 `shouldBe` ExitFailure 1


    it "TRANS_DELETE_01 - Cannot forget pending transaction when not pending anymore via CLI" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        let wSrcId = T.unpack (wSrc ^. walletId)

        -- post transaction
        txJson <- postTxViaCLI ctx wSrc wDest 1
        verify txJson
            [ expectCliField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectCliField (#status . #getApiT) (`shouldBe` Pending)
            ]
        let txId =  getTxId txJson

        eventually "Tx is in ledger" $ do
            (fromStdout <$> listTransactionsViaCLI @t ctx [wSrcId])
                >>= expectValidJSON (Proxy @([ApiTransaction n]))
                >>= flip verify
                    [ expectCliListField 0
                        (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectCliListField 0
                        (#status . #getApiT) (`shouldBe` InLedger)
                    ]

     -- Try Forget transaction once it's no longer pending
        (Exit c2, Stdout out2, Stderr err2) <-
            deleteTransactionViaCLI @t ctx wSrcId txId
        err2 `shouldContain` errMsg403NoPendingAnymore (T.pack txId)
        out2 `shouldBe` ""
        c2 `shouldBe` ExitFailure 1

    it "TRANS_DELETE_03 - Cannot forget tx that is not found via CLI" $ \ctx -> do
        wid <- fixtureWallet' ctx
        let txId = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
        -- forget transaction
        (Exit c, Stdout out, Stderr err) <-
            deleteTransactionViaCLI @t ctx wid (T.unpack txId)
        err `shouldContain` errMsg404CannotFindTx txId
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    describe "TRANS_DELETE_04 - False wallet ids via CLI" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> do
            let txId = "3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12"
            -- forget transaction once again
            (Exit c, Stdout out, Stderr err) <-
                deleteTransactionViaCLI @t ctx walId (T.unpack txId)
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1
            if (title == "40 chars hex") then
                err `shouldContain` "I couldn't find a wallet with \
                    \the given id: 1111111111111111111111111111111111111111"
            else
                err `shouldContain` "wallet id should be a \
                    \hex-encoded string of 40 characters"

    it "TRANS_DELETE_06 -\
        \ Cannot forget tx that is performed from different wallet via CLI"
        $ \ctx -> do
            -- post tx
            wSrc <- fixtureWallet ctx
            wDest <- emptyWallet ctx
            txJson <- postTxViaCLI ctx wSrc wDest 1

            -- try to forget from different wallet
            widDiff <- emptyWallet' ctx
            let txId = getTxId txJson
            (Exit c, Stdout out, Stderr err) <-
                deleteTransactionViaCLI @t ctx widDiff txId
            err `shouldContain` errMsg404CannotFindTx (T.pack txId)
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_DELETE_07 - invalid tx id via CLI" $ do
        let txIds =
                [ replicate 63 '1'
                , replicate 65 '1'
                , replicate 64 'ś'
                ]
        forM_ txIds $ \tid -> it (show tid) $ \ctx -> do
            wid <- emptyWallet' ctx
            (Exit c, Stdout out, Stderr err) <-
                deleteTransactionViaCLI @t ctx wid tid
            err `shouldContain`
                "should be a hex-encoded string of 64 characters"
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    it "BYRON_TX_LIST_03 -\
        \ Shelley CLI does not list Byron wallet transactions" $ \ctx -> do
        wid <- emptyRandomWallet' ctx
        (Exit c, Stdout o, Stderr e) <- listTransactionsViaCLI @t ctx [wid]
        e `shouldContain` errMsg404NoWallet (T.pack wid)
        o `shouldBe` mempty
        c `shouldBe` ExitFailure 1

    it "BYRON_TRANS_DELETE -\
        \ Cannot delete tx on Byron wallet using shelley CLI" $ \ctx -> do
        wid <- emptyRandomWallet' ctx
        (Exit c, Stdout o, Stderr e)
            <- deleteTransactionViaCLI @t ctx wid (replicate 64 '1')
        e `shouldContain` errMsg404NoWallet (T.pack wid)
        o `shouldBe` mempty
        c `shouldBe` ExitFailure 1

    describe "BYRON_TRANS_CREATE / BYRON_TRANS_ESTIMATE -\
        \ Cannot create/estimate tx on Byron wallet using shelley CLI" $ do
        forM_ ["create", "fees"] $ \action -> it action $ \ctx -> do
            wSrc <- emptyRandomWallet ctx
            wDest <- emptyWallet ctx
            addrs:_ <- listAddresses @n ctx wDest
            let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
            let port = T.pack $ show $ ctx ^. typed @(Port "wallet")
            let args = T.unpack <$>
                    [ "transaction", T.pack action, "--port", port
                    , wSrc ^. walletId, "--payment", "11@" <> addr
                    ]
            -- make sure CLI returns error before asking for passphrase
            (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI @t args
            err `shouldContain` "I couldn't find a wallet with \
                \the given id: " ++ T.unpack ( wSrc ^. walletId )
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1
  where
      postTxViaCLI
          :: Context t
          -> ApiWallet
          -> ApiWallet
          -> Natural
          -> IO (ApiTransaction n)
      postTxViaCLI ctx wSrc wDest amt = do
          addr:_ <- listAddresses @n ctx wDest
          let addrStr = encodeAddress @n (getApiT $ fst $ addr ^. #id)
          let args = T.unpack <$>
                  [ wSrc ^. walletId
                  , "--payment", T.pack (show amt) <> "@" <> addrStr
                  ]

          -- post transaction
          (c, out, err) <- postTransactionViaCLI @t ctx "cardano-wallet" args
          err `shouldBe` "Please enter your passphrase: **************\nOk.\n"
          c `shouldBe` ExitSuccess
          expectValidJSON (Proxy @(ApiTransaction n)) out

      fixtureWallet' :: Context t -> IO String
      fixtureWallet' = fmap (T.unpack . view walletId) . fixtureWallet

      emptyWallet' :: Context t -> IO String
      emptyWallet' = fmap (T.unpack . view walletId) . emptyWallet

      emptyRandomWallet' :: Context t -> IO String
      emptyRandomWallet' = fmap (T.unpack . view walletId) . emptyRandomWallet

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

      encodeErr = "Unrecognized address encoding"
      encodeErr2 = "Unable to decode address"
      parseErr = "Parse error. Expecting format \"<amount>@<address>\""
      matrixInvalidAddrs =
-- TODO: For the haskell node, hex is valid. For jormungandr it is not.
--  longAddr = replicate 10000 '1'
--  We should optimally find a way to test this.
--          [ ( "long hex", longAddr, encodeErr )
--          , ( "short hex", "1", encodeErr )
          [ ( "-1000", "-1000", encodeErr )
          , ( "q", "q", encodeErr2 )
          , ( "empty", "", encodeErr2 )
          , ( "wildcards", T.unpack wildcardsWalletName, parseErr )
          , ( "arabic", T.unpack arabicWalletName, encodeErr )
          , ( "kanji", T.unpack kanjiWalletName, encodeErr )
          , ( "polish", T.unpack polishWalletName, encodeErr )
          , ( "[]", "[]", encodeErr )
          , ( "no address", "", encodeErr2 )
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
