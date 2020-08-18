{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.CLI.Shelley.HWWallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiFee
    , ApiTransaction
    , ApiUtxoStatistics
    , ApiWallet
    , DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeAddress (..)
    , encodeAddress
    , getApiT
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( defaultAddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( AddressState (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand
    , createWalletFromPublicKeyViaCLI
    , createWalletViaCLI
    , deleteWalletViaCLI
    , emptyWallet
    , eventually
    , expectCliField
    , expectCliListField
    , expectValidJSON
    , expectWalletUTxO
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWithMnemonics
    , generateMnemonicsViaCLI
    , getWalletUtxoStatisticsViaCLI
    , getWalletViaCLI
    , listAddresses
    , listAddressesViaCLI
    , listTransactionsViaCLI
    , listWalletsViaCLI
    , postTransactionFeeViaCLI
    , postTransactionViaCLI
    , pubKeyFromMnemonics
    , updateWalletNameViaCLI
    , updateWalletPassphraseViaCLI
    , verify
    , walletId
    , (.>)
    )
import Test.Integration.Framework.TestData
    ( cmdOk, errMsg403NoRootKey )
import Test.Integration.Scenario.CLI.Shelley.Wallets
    ( walletNames, walletNamesInvalid )

import qualified Data.Text as T

spec :: forall n t.
    ( KnownCommand t
    , DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    ) => SpecWith (Context t)
spec = do

    it "HW_WALLETS_01 - Restoration from account public key preserves funds" $ \ctx -> do
        wSrc <- fixtureWallet ctx

        -- create a wallet
        Stdout m <- generateMnemonicsViaCLI @t []
        (c1, o1, e1) <- createWalletViaCLI @t ctx ["n"] m "\n" "secure-passphrase"
        c1 `shouldBe` ExitSuccess
        T.unpack e1 `shouldContain` cmdOk
        wDest <- expectValidJSON (Proxy @ApiWallet) o1
        verify wDest
            [ expectCliField
                    (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
            , expectCliField
                    (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
            ]

        --send transaction to the wallet
        let amount = 11
        addrs:_ <- listAddresses @n ctx wDest
        let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amount) <> "@" <> addr
                ]

        (cp, op, ep) <- postTransactionViaCLI @t ctx "cardano-wallet" args
        T.unpack ep `shouldContain` cmdOk
        _ <- expectValidJSON (Proxy @(ApiTransaction n)) op
        cp `shouldBe` ExitSuccess

        eventually "Wallet balance is as expected" $ do
            Stdout og <- getWalletViaCLI @t ctx $ T.unpack (wDest ^. walletId)
            jg <- expectValidJSON (Proxy @ApiWallet) og
            expectCliField (#balance . #getApiT . #available)
                (`shouldBe` Quantity amount) jg
            expectCliField (#balance . #getApiT . #total)
                (`shouldBe` Quantity amount) jg

        -- delete wallet
        Exit cd <- deleteWalletViaCLI @t ctx $ T.unpack (wDest ^. walletId)
        cd `shouldBe` ExitSuccess

        -- restore wallet from account public key
        let accXPub = pubKeyFromMnemonics' (words m)
        (Exit c2, Stdout o2, Stderr e2) <-
            createWalletFromPublicKeyViaCLI @t ctx [restoredWalletName, accXPub]
        c2 `shouldBe` ExitSuccess
        e2 `shouldContain` cmdOk
        wRestored <- expectValidJSON (Proxy @ApiWallet) o2

        -- make sure funds are there
        eventually "Wallet balance is as expected on wallet from pubKey" $ do
            Stdout o3 <- getWalletViaCLI @t ctx $ T.unpack (wRestored ^. walletId)
            justRestored <- expectValidJSON (Proxy @ApiWallet) o3
            verify justRestored
                [ expectCliField
                        (#balance . #getApiT . #available)
                        (`shouldBe` Quantity amount)
                , expectCliField
                        (#balance . #getApiT . #total)
                        (`shouldBe` Quantity amount)
                ]

    describe "HW_WALLETS_03 - Cannot do operations requiring private key" $ do
        it "Cannot send tx" $ \ctx -> do
            -- create wallet from pubKey with funds
            (w, mnemonics) <- fixtureWalletWithMnemonics ctx
            let pubKey = T.unpack $ pubKeyFromMnemonics mnemonics
            Exit cd <- deleteWalletViaCLI @t ctx $ T.unpack (w ^. walletId)
            cd `shouldBe` ExitSuccess

            (Exit c2, Stdout o2, Stderr e2) <-
                createWalletFromPublicKeyViaCLI @t ctx [restoredWalletName, pubKey]
            c2 `shouldBe` ExitSuccess
            e2 `shouldContain` cmdOk
            wRestored <- expectValidJSON (Proxy @ApiWallet) o2

            -- make sure you cannot send tx from wallet
            wDest <- emptyWallet ctx
            addrs:_ <- listAddresses @n ctx wDest
            let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
            let args = T.unpack <$>
                    [ wRestored ^. walletId
                    , "--payment", "1@" <> addr
                    ]

            (c, out, err) <- postTransactionViaCLI @t ctx (T.unpack fixturePassphrase) args
            (T.unpack err)
                `shouldContain` errMsg403NoRootKey (wRestored ^. walletId)
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1

        it "Cannot update pass" $ \ctx -> do
            w <- emptyWalletFromPubKeyViaCLI ctx restoredWalletName

            let pass = "cardano-wallet"
            let wid = T.unpack $ w ^. walletId
            -- cannot update pass
            (exitCode, out, err) <-
                updateWalletPassphraseViaCLI @t ctx wid pass pass pass
            out `shouldBe` ""
            T.unpack err `shouldContain` errMsg403NoRootKey (w ^. walletId)
            exitCode `shouldBe` ExitFailure 1

    describe "HW_WALLETS_04 - Can manage HW wallet the same way as others" $ do
        it "Can update name" $ \ctx -> do
            w <- emptyWalletFromPubKeyViaCLI ctx restoredWalletName

            -- can update wallet name
            let n = "new name"
            let args = T.unpack <$> [w ^. walletId, n]
            (Exit code, Stdout out, Stderr err) <-
                updateWalletNameViaCLI @t ctx args
            code `shouldBe` ExitSuccess
            err `shouldBe` cmdOk
            j <- expectValidJSON (Proxy @ApiWallet) out
            expectCliField
                (#name . #getApiT . #getWalletName) (`shouldBe` n) j

        it "Can get tx fee" $ \ctx -> do
            -- create wallet from pubKey with funds
            (w, mnemonics) <- fixtureWalletWithMnemonics ctx
            let pubKey = T.unpack $ pubKeyFromMnemonics mnemonics
            Exit cd <- deleteWalletViaCLI @t ctx $ T.unpack (w ^. walletId)
            cd `shouldBe` ExitSuccess

            (Exit c1, Stdout o1, Stderr e1) <-
                createWalletFromPublicKeyViaCLI @t ctx [restoredWalletName, pubKey]
            c1 `shouldBe` ExitSuccess
            e1 `shouldContain` cmdOk
            wRestored <- expectValidJSON (Proxy @ApiWallet) o1

            -- get fee
            wDest <- emptyWallet ctx
            addrs:_ <- listAddresses @n ctx wDest
            let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
            let amt = 1 :: Int
            let args = T.unpack <$>
                    [ wRestored ^. walletId
                    , "--payment", T.pack (show amt) <> "@" <> addr
                    ]
            (Exit code, Stdout out, Stderr err) <- postTransactionFeeViaCLI @t ctx args
            err `shouldBe` cmdOk
            txJson <- expectValidJSON (Proxy @ApiFee) out
            verify txJson
                [ expectCliField (#estimatedMin . #getQuantity) (.> 0)
                , expectCliField (#estimatedMax . #getQuantity) (.> 0)
                ]
            code `shouldBe` ExitSuccess

        it "Can delete" $ \ctx -> do
            w <- emptyWalletFromPubKeyViaCLI ctx restoredWalletName
            (Exit cd, Stdout od, Stderr ed) <-
                deleteWalletViaCLI @t ctx $ T.unpack (w ^. walletId)
            cd `shouldBe` ExitSuccess
            ed `shouldContain` cmdOk
            od `shouldBe` "\n"

        it "Can see utxo" $ \ctx -> do
            w <- emptyWalletFromPubKeyViaCLI ctx restoredWalletName
            (Exit c, Stdout o, Stderr e) <-
                getWalletUtxoStatisticsViaCLI @t ctx $ T.unpack (w ^. walletId)
            c `shouldBe` ExitSuccess
            e `shouldBe` cmdOk
            utxoStats <- expectValidJSON (Proxy @ApiUtxoStatistics) o
            expectWalletUTxO [] (Right utxoStats)

        it "Can list addresses" $ \ctx -> do
            w <- emptyWalletFromPubKeyViaCLI ctx restoredWalletName

            let g = fromIntegral $ getAddressPoolGap defaultAddressPoolGap
            (Exit c, Stdout out, Stderr err) <-
                listAddressesViaCLI @t ctx [T.unpack (w ^. walletId)]
            err `shouldBe` "Ok.\n"
            c `shouldBe` ExitSuccess
            json <- expectValidJSON (Proxy @[ApiAddress n]) out
            length json `shouldBe` g
            forM_ [0..(g-1)] $ \addrNum -> do
                expectCliListField
                    addrNum (#state . #getApiT) (`shouldBe` Unused) json

        it "Can have address pool gap" $ \ctx -> do
            Stdout m <- generateMnemonicsViaCLI @t []
            let accXPub = pubKeyFromMnemonics' (words m)
            let addrPoolGap = 55 -- arbitrary but known
            let args =
                    [ restoredWalletName
                    , "--address-pool-gap", show addrPoolGap
                    , accXPub
                    ]
            (Exit c, Stdout out, Stderr err) <-
                createWalletFromPublicKeyViaCLI @t ctx args
            c `shouldBe` ExitSuccess
            err `shouldContain` cmdOk
            j <- expectValidJSON (Proxy @ApiWallet) out
            expectCliField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap)
                    (`shouldBe` addrPoolGap) j

        it "Can list transactions" $ \ctx -> do
            w <- emptyWalletFromPubKeyViaCLI ctx restoredWalletName

            (Exit code, Stdout out, Stderr err) <-
                listTransactionsViaCLI @t ctx [T.unpack $ w ^. walletId]

            err `shouldBe` cmdOk
            out `shouldBe` "[]\n"
            code `shouldBe` ExitSuccess

    describe "HW_WALLETS_05 - Wallet from pubKey is available" $ do
        it "The same account and mnemonic wallet can live side-by-side" $ \ctx -> do
            Stdout m <- generateMnemonicsViaCLI @t []
            let pubKeyWalName = "pub key wallet"
            let mnemonicWalName = "mnemonic wallet"
            -- create wallet from mnemonics
            (c1, o1, e1) <- createWalletViaCLI @t ctx [mnemonicWalName] m "\n" "secure-passphrase"
            c1 `shouldBe` ExitSuccess
            T.unpack e1 `shouldContain` cmdOk
            _ <- expectValidJSON (Proxy @ApiWallet) o1

            -- create wallet from pub key
            let accXPub = pubKeyFromMnemonics' (words m)
            (Exit c2, Stdout o2, Stderr e2) <-
                createWalletFromPublicKeyViaCLI @t ctx [pubKeyWalName, accXPub]
            c2 `shouldBe` ExitSuccess
            e2 `shouldContain` cmdOk
            _ <- expectValidJSON (Proxy @ApiWallet) o2

            (Exit c, Stdout out, Stderr err) <- listWalletsViaCLI @t ctx
            c `shouldBe` ExitSuccess
            err `shouldBe` cmdOk
            rl <- expectValidJSON (Proxy @[ApiWallet]) out
            length rl `shouldBe` 2
            verify rl
                [ expectCliListField 0
                    (#name . #getApiT . #getWalletName)
                    (`shouldBe` T.pack mnemonicWalName)
                , expectCliListField 1
                    (#name . #getApiT . #getWalletName)
                    (`shouldBe` T.pack pubKeyWalName)
                ]

    describe "HW_WALLETS_06 - Test parameters" $ do
        describe "Wallet names valid" $ do
            forM_ walletNames $ \(title, n) -> it title $ \ctx -> do
                j <- emptyWalletFromPubKeyViaCLI ctx n
                expectCliField
                    (#name . #getApiT . #getWalletName) (`shouldBe` T.pack n) j
        describe "Wallet names invalid" $ do
            forM_ walletNamesInvalid $ \(name, expects) -> it expects $ \ctx -> do
                Stdout m <- generateMnemonicsViaCLI @t []
                let accXPub = pubKeyFromMnemonics' (words m)
                (Exit c, Stdout o, Stderr e) <-
                    createWalletFromPublicKeyViaCLI @t ctx [name, accXPub]
                c `shouldBe` ExitFailure 1
                e `shouldContain` expects
                o `shouldBe` mempty
        describe "Pub Key invalid" $ do
            let pubKeysInvalid = ["", "1", replicate 128 'ś', replicate 129 '1']
            forM_ pubKeysInvalid $ \key -> it ("Pub key: " ++ key) $ \ctx -> do
                (Exit c, Stdout o, Stderr e) <-
                    createWalletFromPublicKeyViaCLI @t ctx [restoredWalletName, key]
                c `shouldBe` ExitFailure 1
                e `shouldContain`
                    "Invalid account public key: expecting a hex-encoded value\
                    \ that is 64 bytes in length."
                o `shouldBe` mempty
        describe "Address pool gap invalid" $ do
            let addrPoolMin = fromIntegral @_ @Int $ getAddressPoolGap minBound
            let addrPoolMax = fromIntegral @_ @Int $ getAddressPoolGap maxBound

            let poolGapsInvalid = [-1, 0, addrPoolMin - 1, addrPoolMax + 1]
            forM_ poolGapsInvalid $ \pGap -> it ("Pool gap: " ++ show pGap) $ \ctx -> do
                Stdout m <- generateMnemonicsViaCLI @t []
                let accXPub = pubKeyFromMnemonics' (words m)
                (Exit c, Stdout o, Stderr e) <-
                    createWalletFromPublicKeyViaCLI @t ctx
                        [ restoredWalletName
                        , "--address-pool-gap", show pGap
                        , accXPub]
                c `shouldBe` ExitFailure 1
                e `shouldContain`
                    "option --address-pool-gap: An address pool gap must be a\
                    \ natural number between 10 and 100."
                o `shouldBe` mempty

emptyWalletFromPubKeyViaCLI
    :: forall t. (KnownCommand t)
    => Context t
    -> String
    -> IO ApiWallet
emptyWalletFromPubKeyViaCLI ctx name = do
    Stdout m <- generateMnemonicsViaCLI @t []
    let accXPub = pubKeyFromMnemonics' (words m)
    (Exit c, Stdout o, Stderr e) <-
        createWalletFromPublicKeyViaCLI @t ctx [name, accXPub]
    c `shouldBe` ExitSuccess
    e `shouldContain` cmdOk
    expectValidJSON (Proxy @ApiWallet) o

pubKeyFromMnemonics' :: [String] -> String
pubKeyFromMnemonics' m = T.unpack $ pubKeyFromMnemonics (T.pack <$> m)

restoredWalletName :: String
restoredWalletName = "Wallet from pub key"
