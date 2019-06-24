{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiTransaction, ApiWallet, getApiT )
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
    , fixtureWallet
    , fixtureWalletWith
    , getWalletViaCLI
    , listAddresses
    , postTransactionViaCLI
    , status
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , errMsg403Fee
    , errMsg403InvalidTransaction
    , errMsg403NotEnoughMoney
    , errMsg403UTxO
    , errMsg403WrongPass
    , falseWalletIds
    , kanjiWalletName
    , polishWalletName
    , wildcardsWalletName
    )

import qualified Data.Text as T

spec :: forall t. (EncodeAddress t, DecodeAddress t) => SpecWith (Context t)
spec = do
    it "TRANS_CREATE_01 - Can create transaction via CLI" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest
        let addrStr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addr ^. #id)
        let amt = 14
        let (feeMin, feeMax) = (168609, 168785)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addrStr
                ]

        -- post transaction
        (c, out, err) <- postTransactionViaCLI ctx "cardano-wallet" args
        err `shouldBe` "Please enter a passphrase: **************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction t)) out
        verify txJson
            [ expectCliFieldBetween amount (feeMin + amt, feeMax + amt)
            , expectCliFieldEqual direction Outgoing
            , expectCliFieldEqual status Pending
            ]
        c `shouldBe` ExitSuccess

        -- verify balance on src wallet
        Stdout gOutSrc <- getWalletViaCLI ctx (T.unpack (wSrc ^. walletId))
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
        Stdout gOutDest <- getWalletViaCLI ctx (T.unpack (wDest ^. walletId))
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
        let (feeMin, feeMax) = (181487, 181839)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addr1
                , "--payment", T.pack (show amt) <> "@" <> addr2
                ]

        -- post transaction
        (c, out, err) <- postTransactionViaCLI ctx "cardano-wallet" args
        err `shouldBe` "Please enter a passphrase: **************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction t)) out
        verify txJson
            [ expectCliFieldBetween amount (feeMin + amt, feeMax + amt)
            , expectCliFieldEqual direction Outgoing
            , expectCliFieldEqual status Pending
            ]
        c `shouldBe` ExitSuccess

        -- verify balance on src wallet
        Stdout gOutSrc <- getWalletViaCLI ctx (T.unpack (wSrc ^. walletId))
        gJson <- expectValidJSON (Proxy @ApiWallet) gOutSrc
        verify gJson
            [ expectCliFieldBetween balanceTotal
                ( faucetAmt - feeMax - amt
                , faucetAmt - feeMin - amt
                )
            , expectCliFieldEqual balanceAvailable (faucetAmt - 2*faucetUtxoAmt)
            ]

        expectEventually' ctx balanceAvailable (2*amt) wDest
        expectEventually' ctx balanceTotal (2*amt) wDest

        -- verify balance on dest wallet
        Stdout gOutDest <- getWalletViaCLI ctx (T.unpack (wDest ^. walletId))
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
        let (feeMin, feeMax) = (181487, 181839)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amt) <> "@" <> addr1'
                , "--payment", T.pack (show amt) <> "@" <> addr2'
                ]

        -- post transaction
        (c, out, err) <- postTransactionViaCLI ctx "cardano-wallet" args
        err `shouldBe` "Please enter a passphrase: **************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction t)) out
        verify txJson
            [ expectCliFieldBetween amount (feeMin + amt, feeMax + amt)
            , expectCliFieldEqual direction Outgoing
            , expectCliFieldEqual status Pending
            ]
        c `shouldBe` ExitSuccess

        -- verify balance on src wallet
        Stdout gOutSrc <- getWalletViaCLI ctx (T.unpack (wSrc ^. walletId))
        gJson <- expectValidJSON (Proxy @ApiWallet) gOutSrc
        verify gJson
            [ expectCliFieldBetween balanceTotal
                ( faucetAmt - feeMax - amt
                , faucetAmt - feeMin - amt
                )
            , expectCliFieldEqual balanceAvailable (faucetAmt - 2*faucetUtxoAmt)
            ]

        forM_ [wDest1, wDest2] $ \wDest -> do
            expectEventually' ctx balanceAvailable amt wDest
            expectEventually' ctx balanceTotal amt wDest

            -- verify balance on dest wallets
            Stdout gOutDest <- getWalletViaCLI ctx (T.unpack (wDest ^. walletId))
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

        (c, out, err) <- postTransactionViaCLI ctx "cardano-wallet" args
        (T.unpack err) `shouldContain` errMsg403UTxO
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_03 - 0 balance after transaction" $ \ctx -> do
        let balance = 168434
        wSrc <- fixtureWalletWith ctx [balance]
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses ctx wDest
        let addr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "1@" <> addr
                ]

        (c, out, err) <- postTransactionViaCLI ctx "Secure Passphrase" args
        err `shouldBe` "Please enter a passphrase: *****************\nOk.\n"
        txJson <- expectValidJSON (Proxy @(ApiTransaction t)) out
        verify txJson
            [ expectCliFieldEqual amount balance
            , expectCliFieldEqual direction Outgoing
            , expectCliFieldEqual status Pending
            ]
        c `shouldBe` ExitSuccess

        Stdout gOutSrc <- getWalletViaCLI ctx (T.unpack (wSrc ^. walletId))
        gJson <- expectValidJSON (Proxy @ApiWallet) gOutSrc
        verify gJson
            [ expectCliFieldEqual balanceTotal 0
            , expectCliFieldEqual balanceAvailable 0
            ]

        expectEventually' ctx balanceAvailable 1 wDest
        expectEventually' ctx balanceTotal 1 wDest

        Stdout gOutDest <- getWalletViaCLI ctx (T.unpack (wDest ^. walletId))
        destJson <- expectValidJSON (Proxy @ApiWallet) gOutDest
        verify destJson
            [ expectCliFieldEqual balanceAvailable 1
            , expectCliFieldEqual balanceTotal 1
            ]

    it "TRANS_CREATE_04 - Can't cover fee" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [100_000]
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses ctx wDest
        let addr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "1@" <> addr
                ]

        (c, out, err) <- postTransactionViaCLI ctx "Secure Passphrase" args
        (T.unpack err) `shouldContain` errMsg403Fee
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_04 - Not enough money" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [100_000, 1000]
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses ctx wDest
        let addr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "1000000@" <> addr
                ]

        (c, out, err) <- postTransactionViaCLI ctx "Secure Passphrase" args
        (T.unpack err) `shouldContain` (errMsg403NotEnoughMoney 101_000 1000_000)
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

        (c, out, err) <- postTransactionViaCLI ctx "This password is wrong" args
        (T.unpack err) `shouldContain` errMsg403WrongPass
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    describe "TRANS_CREATE_05 - Invalid addresses" $ do
        let longAddr = replicate 10000 '1'
        let byronErr = "Unable to decode Address: not a valid Byron address."
        let base58Err = "Unable to decode Address: expected Base58 encoding."
        let parseErr = "Parse error. Expecting format \"<amount>@<address>\""
        let matrix =
                [ ( "long hex", longAddr, byronErr )
                , ( "short hex", "1", byronErr )
                , ( "-1000", "-1000", base58Err ), ( "q", "q", byronErr )
                , ( "empty", "", byronErr )
                , ( "wildcards", T.unpack wildcardsWalletName, parseErr )
                , ( "arabic", T.unpack arabicWalletName, base58Err )
                , ( "kanji", T.unpack kanjiWalletName, base58Err )
                , ( "polish", T.unpack polishWalletName, base58Err )
                , ( "[]", "[]", base58Err )
                , ( "no address", "", byronErr )
                , ( "address is space", " ", base58Err )
                ]
        forM_ matrix $ \(title, addr, errMsg) -> it title $ \ctx -> do
            wSrc <- emptyWallet ctx
            let args = T.unpack <$>
                    [ wSrc ^. walletId
                    , "--payment", "12@" <> (T.pack addr)
                    ]

            (c, out, err) <- postTransactionViaCLI ctx "Secure Passphrase" args
            (T.unpack err) `shouldContain` errMsg
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

    describe "TRANS_CREATE_06 - Invalid amount" $ do
        let errNum = "Expecting natural number"
        let parseErr = "Parse error. Expecting format \"<amount>@<address>\""
        let matrix =
                [ ("1.5", "1.5", errNum)
                , ("-1000", "-1000", errNum)
                , ("[]", "[]", errNum)
                , ("string with diacritics", polishWalletName, errNum)
                , ("string with wildcards", wildcardsWalletName, parseErr)
                , ("no amount", "", errNum)
                ]
        forM_ matrix $ \(title, amt, errMsg) -> it title $ \ctx -> do
            wSrc <- emptyWallet ctx
            wDest <- emptyWallet ctx
            addrs:_ <- listAddresses ctx wDest
            let addr =
                    encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
            let args = T.unpack <$>
                    [ wSrc ^. walletId
                    , "--payment", amt <> "@" <> addr
                    ]

            (c, out, err) <- postTransactionViaCLI ctx "cardano-wallet" args
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
            (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI args
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
        (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI args
        err `shouldContain` "wallet id should be an hex-encoded\
            \ string of 40 characters"
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_07 - Deleted wallet" $ \ctx -> do
        wSrc <- emptyWallet ctx
        Exit ex <- deleteWalletViaCLI ctx (T.unpack ( wSrc ^. walletId ))
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
        (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI args
        err `shouldContain` "I couldn't find a wallet with \
            \the given id: " ++ T.unpack ( wSrc ^. walletId )
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_09 - 0 amount transaction is forbidden on single output tx" $ \ctx -> do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addrs:_ <- listAddresses ctx wDest
        let addr =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs ^. #id)
        let amt = "0"
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", amt <> "@" <> addr
                ]

        (c, out, err) <- postTransactionViaCLI ctx "cardano-wallet" args
        (T.unpack err) `shouldContain` errMsg403InvalidTransaction
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1

    it "TRANS_CREATE_09 - 0 amount transaction is forbidden on multi-output tx" $ \ctx -> do
        wSrc <- fixtureWalletWith ctx [10_000_000, 10_000_000]
        wDest <- emptyWallet ctx
        addrs <- listAddresses ctx wDest
        let addr1 =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs !! 1 ^. #id)
        let addr2 =
                encodeAddress (Proxy @t) (getApiT $ fst $ addrs !! 2 ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", "0@" <> addr1
                , "--payment", "15@" <> addr2
                ]

        (c, out, err) <- postTransactionViaCLI ctx "Secure Passphrase" args
        (T.unpack err) `shouldContain` errMsg403InvalidTransaction
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1
