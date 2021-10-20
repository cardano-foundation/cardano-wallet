{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.CLI.Shelley.Wallets
    ( spec
    , walletNames
    , walletNamesInvalid
    ) where

import Prelude

import Cardano.CLI
    ( Port )
import Cardano.Wallet.Api.Types
    ( ApiTransaction
    , ApiUtxoStatistics
    , ApiWallet
    , ApiWalletUtxoSnapshot
    , DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeAddress (..)
    , getApiT
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap (..) )
import Cardano.Wallet.Primitive.Passphrase
    ( PassphraseMaxLength (..), PassphraseMinLength (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( getWalletName, walletNameMaxLength, walletNameMinLength )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( MonadIO )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..) )
import Control.Monad.Trans.Resource
    ( ResourceT, runResourceT )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Product.Typed
    ( typed )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import Numeric.Natural
    ( Natural )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldNotBe, shouldNotContain )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , cardanoWalletCLI
    , createWalletViaCLI
    , deleteWalletViaCLI
    , emptyRandomWallet
    , emptyWallet
    , emptyWalletWith
    , eventually
    , expectCliField
    , expectCliListField
    , expectValidJSON
    , expectWalletUTxO
    , fixtureWallet
    , generateMnemonicsViaCLI
    , getWalletUtxoSnapshotViaCLI
    , getWalletUtxoStatisticsViaCLI
    , getWalletViaCLI
    , listAddresses
    , listWalletsViaCLI
    , minUTxOValue
    , notDelegating
    , postTransactionViaCLI
    , updateWalletNameViaCLI
    , updateWalletPassphraseViaCLI
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , cmdOk
    , errMsg400WalletIdEncoding
    , errMsg403WrongPass
    , errMsg404NoWallet
    , falseWalletIds
    , russianWalletName
    , wildcardsWalletName
    )

import qualified Data.Text as T

spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    ) => SpecWith Context
spec = describe "SHELLEY_CLI_WALLETS" $ do
    it "BYRON_GET_03 - Shelley CLI does not show Byron wallet" $ \ctx -> runResourceT $ do
        wid <- emptyRandomWallet' ctx
        (Exit c, Stdout out, Stderr err) <- getWalletViaCLI ctx wid
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1
        err `shouldContain` errMsg404NoWallet (T.pack wid)

    it "BYRON_LIST_03 - Shelley CLI does not list Byron wallet" $ \ctx -> runResourceT $ do
        byronWid <- emptyRandomWallet' ctx
        shelleyWid <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- listWalletsViaCLI ctx
        c `shouldBe` ExitSuccess
        err `shouldBe` cmdOk
        j <- expectValidJSON (Proxy @[ApiWallet]) out
        let wids = map (T.unpack . view walletId) j
        wids `shouldContain` [shelleyWid]
        wids `shouldNotContain` [byronWid]

    it "BYRON_DELETE_03 - Shelley CLI does not delete Byron wallet" $ \ctx -> runResourceT $ do
        wid <- emptyRandomWallet' ctx
        (Exit c, Stdout out, Stderr err) <- deleteWalletViaCLI ctx wid
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1
        err `shouldContain` errMsg404NoWallet (T.pack wid)

    it "BYRON_WALLETS_UTXO -\
        \ Cannot show Byron wal utxo with shelley CLI" $ \ctx -> runResourceT $ do
        wid <- emptyRandomWallet' ctx
        (Exit c, Stdout o, Stderr e) <- getWalletUtxoStatisticsViaCLI ctx wid
        c `shouldBe` ExitFailure 1
        e `shouldContain` errMsg404NoWallet (T.pack wid)
        o `shouldBe` mempty

    it "BYRON_WALLETS_UPDATE_PASS -\
        \ Cannot update Byron wal with shelley CLI" $ \ctx -> runResourceT $ do
        wid <- emptyRandomWallet' ctx
        let port = T.pack $ show $ ctx ^. typed @(Port "wallet")
        let args = T.unpack <$>
                [ "wallet", "update", "passphrase"
                , "--port", port, T.pack wid ]
        (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI args
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1
        err `shouldContain` errMsg404NoWallet (T.pack wid)

    it "BYRON_WALLETS_UPDATE -\
        \ Cannot update name Byron wal with shelley CLI" $ \ctx -> runResourceT $ do
        wid <- emptyRandomWallet' ctx
        let port = T.pack $ show $ ctx ^. typed @(Port "wallet")
        let args = T.unpack <$>
                [ "wallet", "update", "name"
                , "--port", port, T.pack wid, "name" ]
        (Exit c, Stdout out, Stderr err) <- cardanoWalletCLI args
        out `shouldBe` ""
        c `shouldBe` ExitFailure 1
        err `shouldContain` errMsg404NoWallet (T.pack wid)

    it "WALLETS_CREATE_01,08 - Can create a wallet" $ \ctx -> runResourceT $ do
        Stdout m <- generateMnemonicsViaCLI []
        (c, out, err) <- createWalletViaCLI ctx ["n"] m "\n" "secure-passphrase"
        c `shouldBe` ExitSuccess
        T.unpack err `shouldContain` cmdOk
        j <- expectValidJSON (Proxy @ApiWallet) out
        verify j
            [ expectCliField (#name . #getApiT . #getWalletName) (`shouldBe` "n")
            , expectCliField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
            , expectCliField (#balance . #available) (`shouldBe` Quantity 0)
            , expectCliField (#balance . #total) (`shouldBe` Quantity 0)
            , expectCliField (#balance . #reward) (`shouldBe` Quantity 0)
            , expectCliField #delegation (`shouldBe` notDelegating [])
            , expectCliField #passphrase (`shouldNotBe` Nothing)
            ]

        eventually "Wallet state = Ready" $ do
            Stdout og <- getWalletViaCLI ctx $ T.unpack (j ^. walletId)
            jg <- expectValidJSON (Proxy @ApiWallet) og
            expectCliField (#state . #getApiT) (`shouldBe` Ready) jg

    it "WALLETS_CREATE_02 - Restored wallet preserves funds" $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx

        -- create a wallet
        Stdout m <- generateMnemonicsViaCLI []
        (c1, o1, e1) <- createWalletViaCLI ctx ["n"] m "\n" "secure-passphrase"
        c1 `shouldBe` ExitSuccess
        T.unpack e1 `shouldContain` cmdOk
        wDest <- expectValidJSON (Proxy @ApiWallet) o1
        verify wDest
            [ expectCliField
                    (#balance . #available) (`shouldBe` Quantity 0)
            , expectCliField
                    (#balance . #total) (`shouldBe` Quantity 0)
            ]

        --send transaction to the wallet
        let amount = (minUTxOValue (_mainEra ctx))
        addrs:_ <- listAddresses @n ctx wDest
        let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)
        let args = T.unpack <$>
                [ wSrc ^. walletId
                , "--payment", T.pack (show amount) <> "@" <> addr
                ]

        (cp, op, ep) <- postTransactionViaCLI ctx "cardano-wallet" args
        T.unpack ep `shouldContain` cmdOk
        _ <- expectValidJSON (Proxy @(ApiTransaction n)) op
        cp `shouldBe` ExitSuccess

        eventually "Wallet balance is as expected" $ do
            Stdout og <- getWalletViaCLI ctx $ T.unpack (wDest ^. walletId)
            jg <- expectValidJSON (Proxy @ApiWallet) og
            expectCliField (#balance . #available)
                (`shouldBe` Quantity amount) jg
            expectCliField (#balance . #total)
                (`shouldBe` Quantity amount) jg

        -- delete wallet
        Exit cd <- deleteWalletViaCLI ctx $ T.unpack (wDest ^. walletId)
        cd `shouldBe` ExitSuccess

        -- restore wallet
        (c2, o2, e2) <- createWalletViaCLI ctx ["n"] m "\n" "secure-passphraseX"
        c2 `shouldBe` ExitSuccess
        T.unpack e2 `shouldContain` cmdOk
        wRestored <- expectValidJSON (Proxy @ApiWallet) o2
        expectCliField walletId (`shouldBe` wDest ^. walletId) wRestored
        eventually "Wallet is fully restored" $ do
            Stdout og2 <- getWalletViaCLI ctx $ T.unpack (wDest ^. walletId)
            jg2 <- expectValidJSON (Proxy @ApiWallet) og2
            expectCliField (#state . #getApiT) (`shouldBe` Ready) jg2

        -- make sure funds are there
        Stdout o3 <- getWalletViaCLI ctx $ T.unpack (wRestored ^. walletId)
        justRestored <- expectValidJSON (Proxy @ApiWallet) o3
        verify justRestored
            [ expectCliField
                    (#balance . #available) (`shouldBe` Quantity amount)
            , expectCliField
                    (#balance . #total) (`shouldBe` Quantity amount)
            , expectCliField walletId (`shouldBe` wDest ^. walletId)
            ]

    it "WALLETS_CREATE_03 - Cannot create wallet that exists" $ \ctx -> runResourceT $ do
        Stdout m1 <- generateMnemonicsViaCLI ["--size", "24"]
        Stdout m2 <- generateMnemonicsViaCLI ["--size", "12"]

        (c1, o1, e1) <- createWalletViaCLI ctx ["n1"] m1 m2 "secure-passphrase"
        c1 `shouldBe` ExitSuccess
        T.unpack e1 `shouldContain` cmdOk
        j <- expectValidJSON (Proxy @ApiWallet) o1
        expectCliField (#name . #getApiT . #getWalletName) (`shouldBe` "n1") j

        (c2, o2, e2) <- createWalletViaCLI ctx ["n2"] m1 m2 "Xsecure-passphraseX"
        c2 `shouldBe` ExitFailure 1
        o2 `shouldBe` ""
        T.unpack e2 `shouldContain` "This operation would yield a wallet with \
            \the following id: " ++ T.unpack (j ^. walletId) ++ " \
            \However, I already know of a wallet with this id."

    describe "WALLETS_CREATE_04 - Wallet names" $ do
        forM_ walletNames $ \(title, n) -> it title $ \ctx -> runResourceT $ do
            Stdout m <- generateMnemonicsViaCLI ["--size", "18"]
            (c, o, e) <- createWalletViaCLI ctx [n] m "\n" "secure-passphrase"
            c `shouldBe` ExitSuccess
            T.unpack e `shouldContain` cmdOk
            j <- expectValidJSON (Proxy @ApiWallet) o
            expectCliField
                (#name . #getApiT . #getWalletName) (`shouldBe` T.pack n) j

    describe "WALLETS_CREATE_04 - Wallet names invalid" $ do
        forM_ walletNamesInvalid $ \(name, expects) -> it expects $ \ctx -> runResourceT $ do
            Stdout m <- generateMnemonicsViaCLI ["--size", "18"]
            (c, o, e) <- createWalletViaCLI ctx [name] m "\n" "secure-passphrase"
            c `shouldBe` ExitFailure 1
            T.unpack e `shouldContain` expects
            o `shouldBe` ""

    describe "WALLETS_CREATE_05 - Can create wallet with different mnemonic sizes" $ do
        forM_ ["15", "18", "21", "24"] $ \(size) -> it size $ \ctx -> runResourceT $ do
            let name = "Wallet created via CLI "
            Stdout mnemonics <- generateMnemonicsViaCLI ["--size", size]
            let pwd = "Secure passphrase"
            (c, out, err) <-
                createWalletViaCLI ctx [name] mnemonics "\n" pwd
            c `shouldBe` ExitSuccess
            T.unpack err `shouldContain` cmdOk
            j <- expectValidJSON (Proxy @ApiWallet) out
            expectCliField
                    (#name . #getApiT . #getWalletName) (`shouldBe` T.pack name) j

    describe "WALLETS_CREATE_05 - Can't create wallet with wrong size of mnemonic" $ do
        forM_ ["9", "12"] $ \(size) -> it size $ \ctx -> runResourceT $ do
            let name = "Wallet created via CLI"
            Stdout m1 <- generateMnemonicsViaCLI ["--size", size]
            Stdout m2 <- generateMnemonicsViaCLI ["--size", size]
            let pwd = "Secure passphrase"
            (c, out, err) <-
                createWalletViaCLI ctx [name] m1 m2 pwd
            c `shouldBe` (ExitFailure 1)
            out `shouldBe` ""
            T.unpack err `shouldContain` "Invalid number of words: 15, 18, 21\
                \ or 24 words are expected."

    describe "WALLETS_CREATE_06 - Can create wallet with different mnemonic snd factor sizes" $ do
        forM_ ["9", "12"] $ \(size) -> it size $ \ctx -> runResourceT $ do
            let name = "Wallet created via CLI"
            Stdout m1 <- generateMnemonicsViaCLI []
            Stdout m2 <- generateMnemonicsViaCLI ["--size", size]
            let pwd = "Secure passphrase"
            (c, out, err) <- createWalletViaCLI ctx [name] m1 m2 pwd
            c `shouldBe` ExitSuccess
            T.unpack err `shouldContain` cmdOk
            j <- expectValidJSON (Proxy @ApiWallet) out
            expectCliField
                    (#name . #getApiT . #getWalletName) (`shouldBe` T.pack name) j

    describe "WALLETS_CREATE_06 - Can't create wallet with wrong size of mnemonic snd factor" $ do
        forM_ ["15", "18", "21", "24"] $ \(size) -> it size $ \ctx -> runResourceT $ do
            let name = "Wallet created via CLI"
            Stdout m1 <- generateMnemonicsViaCLI ["--size", size]
            Stdout m2 <- generateMnemonicsViaCLI ["--size", size]
            let pwd = "Secure passphrase"
            (c, out, err) <- createWalletViaCLI ctx [name] m1 m2 pwd
            c `shouldBe` (ExitFailure 1)
            out `shouldBe` ""
            T.unpack err `shouldContain` "Invalid number of words: 9 or 12\
                \ words are expected."

    describe "WALLETS_CREATE_07 - Passphrase is valid" $ do
        let proxy_ = Proxy @"user"
        let passphraseMax = replicate (passphraseMaxLength proxy_) 'ą'
        let passBelowMax = replicate (passphraseMaxLength proxy_ - 1) 'ć'
        let passphraseMin = replicate (passphraseMinLength proxy_) 'ń'
        let passAboveMin = replicate (passphraseMinLength proxy_ + 1) 'ę'
        let matrix =
                [ ( "Pass min", passphraseMin )
                , ( "Pass max", passphraseMax )
                , ( "Pass max - 1", passBelowMax )
                , ( "Pass min + 1", passAboveMin )
                , ( "Russian", "АаБбВвГгДдЕеЁёЖжЗз")
                , ( "Polish", "aąbcćdeęfghijklłmnoóp" )
                , ( "Kanji", "亜哀挨愛曖悪握圧扱宛嵐")
                ]
        forM_ matrix $ \(title, pass) -> it title $ \ctx -> runResourceT $ do
            Stdout m <- generateMnemonicsViaCLI []
            (c, o, e) <- createWalletViaCLI ctx ["Wallet name"] m "\n" pass
            c `shouldBe` ExitSuccess
            T.unpack e `shouldContain` cmdOk
            j <- expectValidJSON (Proxy @ApiWallet) o
            expectCliField #passphrase (`shouldNotBe` Nothing) j

    describe "WALLETS_CREATE_07 - When passphrase is invalid" $ do
        let proxy_ = Proxy @"user"
        let passAboveMax = replicate (passphraseMaxLength proxy_ + 1) 'ą'
        let passBelowMin = replicate (passphraseMinLength proxy_ - 1) 'ń'
        let passMinWarn = "passphrase is too short: expected at \
            \least " ++ show (passphraseMinLength proxy_) ++ " characters"
        let passMaxWarn = "passphrase is too long: expected at \
            \most " ++ show (passphraseMaxLength proxy_) ++ " characters"

        let matrix =
                [ ( "Pass below min length", passBelowMin, passMinWarn )
                , ( "Pass above max length", passAboveMax, passMaxWarn )
                ]
        forM_ matrix $ \(title, pass, warn) -> it title $ \ctx -> runResourceT $ do
            Stdout m <- generateMnemonicsViaCLI []
            (c, o, e) <- createWalletViaCLI ctx ["Wallet name"] m "\n" pass
            c `shouldBe` ExitFailure 1
            o `shouldBe` ""
            T.unpack e `shouldContain` warn

    describe "WALLETS_CREATE_08 - --address-pool-gap values" $ do
        let expectsOk c o e gap = do
                c `shouldBe` ExitSuccess
                T.unpack e `shouldContain` cmdOk
                j <- expectValidJSON (Proxy @ApiWallet) o
                expectCliField
                        (#addressPoolGap . #getApiT . #getAddressPoolGap)
                        (`shouldBe` (read gap :: Word32))
                        j

        let addrPoolMin = fromIntegral @_ @Int $ getAddressPoolGap minBound
        let addrPoolMax = fromIntegral @_ @Int $ getAddressPoolGap maxBound

        let expectsErr c o e gap = do
                c `shouldBe` ExitFailure 1
                o `shouldNotContain` gap
                T.unpack e `shouldContain`
                    "An address pool gap must be a natural number between "
                    ++ show addrPoolMin ++ " and "
                    ++ show addrPoolMax ++ "."

        let matrix =
                [ ( "Gap min", show addrPoolMin, expectsOk )
                , ( "Gap min + 1", show (addrPoolMin + 1), expectsOk )
                , ( "Gap max + 1 -> fail", show (addrPoolMax + 1), expectsErr )
                , ( "Gap min - 1 -> fail", show (addrPoolMin - 1), expectsErr )
                , ( "-1000 -> fail", "-1000", expectsErr )
                , ( "0 -> fail", "0", expectsErr )
                , ( "10.5 -> fail", "10.5", expectsErr )
                , ( "arbitrary string -> fail", "string", expectsErr )
                ]

        forM_ matrix $ \(title, gap, expects) -> it title $ \ctx -> runResourceT $ do
            Stdout m <- generateMnemonicsViaCLI []
            (c, o, e) <- createWalletViaCLI ctx ["n", "--address-pool-gap", gap]
                            m "\n" "secure-passphraze"
            expects c o e gap

    it "WALLETS_GET_01 - Can get a wallet" $ \ctx -> runResourceT $ do
        walId <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- getWalletViaCLI ctx walId
        c `shouldBe` ExitSuccess
        err `shouldBe` cmdOk
        j <- expectValidJSON (Proxy @ApiWallet) out
        verify j
            [ expectCliField
                    (#name . #getApiT . #getWalletName) (`shouldBe` "Empty Wallet")
            , expectCliField
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 20)
            , expectCliField
                    (#balance . #available) (`shouldBe` Quantity 0)
            , expectCliField
                    (#balance . #total) (`shouldBe` Quantity 0)
            , expectCliField
                    (#balance . #reward) (`shouldBe` Quantity 0)
            , expectCliField #delegation (`shouldBe` notDelegating [])
            , expectCliField #passphrase (`shouldNotBe` Nothing)
            ]

        eventually "Wallet state = Ready" $ do
            Stdout og <- getWalletViaCLI ctx walId
            jg <- expectValidJSON (Proxy @ApiWallet) og
            expectCliField (#state . #getApiT) (`shouldBe` Ready) jg

    describe "WALLETS_GET_03,04 - Cannot get wallets with false ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> runResourceT $ do
            (Exit c, Stdout out, Stderr err) <- getWalletViaCLI ctx walId
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1
            if (title == "40 chars hex") then
                err `shouldContain` errMsg404NoWallet (T.pack $ replicate 40 '1')
            else
                err `shouldContain` errMsg400WalletIdEncoding

    it "WALLETS_LIST_01 - Can list wallets" $ \ctx -> runResourceT $ do
        let prefix = "WALLETS_LIST_01_can"
        let name1 = prefix <> "#1"
        let name2 = prefix <> "#2"
        w1 <- emptyWalletWith' ctx (name1, "secure-passphrase", 21)
        _ <- emptyWalletWith' ctx (name2, "secure-passphrase", 21)
        (Exit c, Stdout out, Stderr err) <- listWalletsViaCLI ctx
        c `shouldBe` ExitSuccess
        err `shouldBe` cmdOk
        let walFromThisTest w =
                prefix `T.isPrefixOf` getWalletName (getApiT (view #name w))
        j <- filter walFromThisTest <$> expectValidJSON (Proxy @[ApiWallet]) out
        length j `shouldBe` 2
        verify j
            [ expectCliListField 0
                    (#name . #getApiT . #getWalletName) (`shouldBe` name1)
            , expectCliListField 0
                    (#addressPoolGap . #getApiT . #getAddressPoolGap) (`shouldBe` 21)
            , expectCliListField 0
                    (#balance . #available) (`shouldBe` Quantity 0)
            , expectCliListField 0
                    (#balance . #total) (`shouldBe` Quantity 0)
            , expectCliListField 0
                    (#balance . #reward) (`shouldBe` Quantity 0)
            , expectCliListField 0 #delegation (`shouldBe` notDelegating [])
            , expectCliListField 0 walletId (`shouldBe` T.pack w1)
            ]

    it "WALLETS_LIST_01 - Wallets are listed from oldest to newest" $ \ctx -> runResourceT $ do
        let prefix = "WALLETS_LIST_01_order"
        w1 <- emptyWalletWith' ctx (prefix <> "1", "secure-passphrase", 20)
        w2 <- emptyWalletWith' ctx (prefix <> "2", "secure-passphrase", 20)
        w3 <- emptyWalletWith' ctx (prefix <> "3", "secure-passphrase", 20)
        (Exit c, Stdout out, Stderr err) <- listWalletsViaCLI ctx
        c `shouldBe` ExitSuccess
        err `shouldBe` cmdOk
        let walFromThisTest w =
                prefix `T.isPrefixOf` getWalletName (getApiT (view #name w))
        j <- filter walFromThisTest <$> expectValidJSON (Proxy @[ApiWallet]) out
        length j `shouldBe` 3
        verify j
            [ expectCliListField 0 walletId (`shouldBe` T.pack w1)
            , expectCliListField 1 walletId (`shouldBe` T.pack w2)
            , expectCliListField 2 walletId (`shouldBe` T.pack w3)
            ]

    describe "WALLETS_UPDATE_01,02 - Can update wallet name" $ do
        forM_ walletNames $ \(title, n) -> it title $ \ctx -> runResourceT $ do
            wid <- emptyWallet' ctx
            let args = [wid, n]
            (Exit c, Stdout out, Stderr err) <-
                updateWalletNameViaCLI ctx args
            c `shouldBe` ExitSuccess
            err `shouldBe` cmdOk
            j <- expectValidJSON (Proxy @ApiWallet) out
            expectCliField
                (#name . #getApiT . #getWalletName) (`shouldBe` T.pack n) j

    it "WALLETS_UPDATE_PASS_01 - Can update passphrase normally"
        $ \ctx -> runResourceT $ do
            let name = "name"
            let ppOld = "old secure passphrase"
            let ppNew = "new secure passphrase"
            let addrPoolMin = fromIntegral @_ @Int $ getAddressPoolGap minBound
            w <- emptyWalletWith ctx (name, T.pack ppOld, addrPoolMin)
            let initPassUpdateTime = w ^. #passphrase
            let wid = T.unpack $ w ^. walletId

            --update pass
            (exitCode, out, err) <-
                updateWalletPassphraseViaCLI ctx wid ppOld ppNew ppNew
            out `shouldBe` "\n"
            T.unpack err `shouldContain` cmdOk
            exitCode `shouldBe` ExitSuccess

            --verify passphraseLastUpdate was updated
            Stdout o <- getWalletViaCLI ctx wid
            j <- expectValidJSON (Proxy @ApiWallet) o
            expectCliField #passphrase (`shouldNotBe` initPassUpdateTime) j

    describe "WALLETS_UPDATE_PASS_02 - New passphrase values" $ do
        let minLength = passphraseMinLength (Proxy @"user")
        let maxLength = passphraseMaxLength (Proxy @"user")
        let matrix =
                [ ( show minLength ++ " char long"
                  , replicate minLength 'ź'
                  , expect (ExitSuccess, "\n", cmdOk)
                  )
                , ( show (minLength - 1) ++ " char long"
                  , replicate (minLength - 1) 'ź'
                  , expect (ExitFailure 1, mempty, "passphrase is too short")
                  )
                , ( show maxLength ++ " char long"
                  , replicate minLength 'ź'
                  , expect (ExitSuccess, "\n", cmdOk)
                  )
                , ( show (maxLength + 1) ++ " char long"
                  , replicate (maxLength + 1) 'ź'
                  , expect (ExitFailure 1, mempty, "passphrase is too long")
                  )
                , ( "Empty passphrase"
                  , ""
                  , expect (ExitFailure 1, mempty, "passphrase is too short")
                  )
                , ( "Russian passphrase", T.unpack russianWalletName
                  , expect (ExitSuccess, "\n", cmdOk)
                  )
                , ( "Arabic passphrase", T.unpack arabicWalletName
                  , expect (ExitSuccess, "\n", cmdOk)
                  )
                , ( "Wildcards passphrase", T.unpack wildcardsWalletName
                  , expect (ExitSuccess, "\n", cmdOk)
                  )
                ]
        forM_ matrix $ \(title, ppNew, expectations) -> it title $ \ctx -> runResourceT $ do
            let name = "name"
            let ppOld = "old secure passphrase"
            let addrPoolMin = fromIntegral @_ @Int $ getAddressPoolGap minBound
            wid <- emptyWalletWith' ctx (name, T.pack ppOld, addrPoolMin)
            (exitCode, out, err) <-
                updateWalletPassphraseViaCLI ctx wid ppOld ppNew ppNew
            expectations (exitCode, out, err)

    it "WALLETS_UPDATE_PASS_02 - \
        \Cannot update passphrase if new passphrase is not confirmed correctly"
        $ \ctx -> runResourceT $ do
            let name = "name"
            let ppOld = "old secure passphrase"
            let ppNew1 = "new secure passphrase 1"
            let ppNew2 = "new secure passphrase 2"
            let addrPoolMin = fromIntegral @_ @Int $ getAddressPoolGap minBound
            wid <- emptyWalletWith' ctx (name, T.pack ppOld, addrPoolMin)
            (exitCode, out, err) <-
                updateWalletPassphraseViaCLI ctx wid ppOld ppNew1 ppNew2
            out `shouldBe` mempty
            T.unpack err `shouldContain` "Passphrases don't match"
            exitCode `shouldBe` ExitFailure 1

    describe "WALLETS_UPDATE_PASS_03 - Old passphrase values" $ do
        let minLength = passphraseMinLength (Proxy @"user")
        let maxLength = passphraseMaxLength (Proxy @"user")
        let matrix =
                [ ( show (minLength - 1) ++ " char long"
                  , replicate (minLength - 1) 'ź'
                  , expect (ExitFailure 1, mempty, "passphrase is too short")
                  )
                , ( show (maxLength + 1) ++ " char long"
                  , replicate (maxLength + 1) 'ź'
                  , expect (ExitFailure 1, mempty, "passphrase is too long")
                  )
                , ( "Empty passphrase"
                  , ""
                  , expect (ExitFailure 1, mempty, "passphrase is too short")
                  )
                , ( "Incorrect old passphrase"
                  , "wrong secure passphrase"
                  , expect (ExitFailure 1, mempty, errMsg403WrongPass)
                  )
                ]
        forM_ matrix $ \(title, ppOldWrong, expectations) -> it title $ \ctx -> runResourceT $ do
            let name = "name"
            let ppOldRight = "right secure passphrase"
            let ppNew = "new secure passphrase"
            let addrPoolMin = fromIntegral @_ @Int $ getAddressPoolGap minBound
            wid <- emptyWalletWith' ctx
                (name, T.pack ppOldRight, addrPoolMin)
            (exitCode, out, err) <-
                updateWalletPassphraseViaCLI ctx wid ppOldWrong ppNew ppNew
            expectations (exitCode, out, err)

    describe "WALLETS_UPDATE_PASS_03 - \
        \Can update pass from pass that's boundary value" $ do
        let minLength = passphraseMinLength (Proxy @"user")
        let maxLength = passphraseMaxLength (Proxy @"user")
        let matrix =
                [ ( show minLength ++ " char long"
                  , replicate minLength 'ź'
                  , expect (ExitSuccess, "\n", cmdOk)
                  )
                , ( show maxLength ++ " char long"
                  , replicate maxLength 'ź'
                 , expect (ExitSuccess, "\n", cmdOk)
                  )
                ]
        forM_ matrix $ \(title, ppOldRight, expectations) -> it title $ \ctx -> runResourceT $ do
            let name = "name"
            let ppNew = replicate maxLength 'ź'
            let addrPoolMin = fromIntegral @_ @Int $ getAddressPoolGap minBound
            wid <- emptyWalletWith' ctx
                (name, T.pack ppOldRight, addrPoolMin)
            (exitCode, out, err) <-
                updateWalletPassphraseViaCLI ctx wid ppOldRight ppNew ppNew
            expectations (exitCode, out, err)

    describe "WALLETS_UPDATE_PASS_04 - Cannot update pass of wallets with false ids" $ do
        forM_ falseWalletIds $ \(title, wid) -> it title $ \ctx -> runResourceT $ do
            let ppOld = "right secure passphrase"
            let ppNew = "new secure passphrase"
            (c, out, err) <-
                updateWalletPassphraseViaCLI ctx wid ppOld ppNew ppNew
            out `shouldBe` ""
            c `shouldBe` ExitFailure 1
            if (title == "40 chars hex") then
                T.unpack err `shouldContain`
                        errMsg404NoWallet (T.pack $ replicate 40 '1')
            else
                T.unpack err `shouldContain` errMsg400WalletIdEncoding

    describe "WALLETS_UPDATE_PASS_05,06 - \
        \Transaction after updating passphrase can only be made with new pass" $ do
        let oldPass = "cardano-wallet"
        let newPass = "cardano-wallet2"
        let expectTxOK (ec, out, err) = do
                ec `shouldBe` ExitSuccess
                _ <- expectValidJSON (Proxy @(ApiTransaction n)) out
                T.unpack err `shouldContain` cmdOk
        let matrix =
                [ ("Old passphrase -> fail", oldPass
                  , expect (ExitFailure 1, mempty, errMsg403WrongPass)
                  )
                , ("New passphrase -> OK", newPass
                  , expectTxOK
                  )
                ]

        forM_ matrix $ \(title, pass, expectations) -> it title $ \ctx -> runResourceT $ do
            wSrc <- fixtureWallet ctx
            wDest <- emptyWallet ctx
            addr:_ <- listAddresses @n ctx wDest
            let wid = T.unpack $ wSrc ^. walletId
            (c, out, err) <-
                updateWalletPassphraseViaCLI ctx wid oldPass newPass newPass
            expect (ExitSuccess, "\n", cmdOk) (c, out, err)

            let addrStr = encodeAddress @n (getApiT $ fst $ addr ^. #id)
            let args = T.unpack <$>
                    [ wSrc ^. walletId
                    , "--payment", T.pack (show (minUTxOValue (_mainEra ctx))) <> "@" <> addrStr
                    ]

            (cTx, outTx, errTx) <- postTransactionViaCLI ctx pass args
            expectations (cTx, outTx, errTx)

    it "WALLETS_DELETE_01, WALLETS_LIST_02 - Can delete wallet" $ \ctx -> runResourceT $ do
        walId <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- deleteWalletViaCLI ctx walId
        err `shouldBe` cmdOk
        c `shouldBe` ExitSuccess
        out `shouldBe` "\n"
        (Stdout o, Stderr e) <- listWalletsViaCLI ctx
        wids <- map (T.unpack . view walletId)
            <$> expectValidJSON (Proxy @[ApiWallet]) o
        wids `shouldNotContain` [walId]
        e `shouldBe` cmdOk

    it "WALLETS_UTXO_01 - Wallet's inactivity is reflected in utxo" $ \ctx -> runResourceT $ do
        wid <- emptyWallet' ctx
        (Exit c, Stdout o, Stderr e) <- getWalletUtxoStatisticsViaCLI ctx wid
        c `shouldBe` ExitSuccess
        e `shouldBe` cmdOk
        utxoStats <- expectValidJSON (Proxy @ApiUtxoStatistics) o
        expectWalletUTxO [] (Right utxoStats)

    it "WALLET_UTXO_SNAPSHOT_01 - \
        \Can generate UTxO snapshot of empty wallet" $
        \ctx -> runResourceT $ do
            wid <- emptyWallet' ctx
            (Exit c, Stdout o, Stderr e) <- getWalletUtxoSnapshotViaCLI ctx wid
            c `shouldBe` ExitSuccess
            e `shouldBe` cmdOk
            utxoSnap <- expectValidJSON (Proxy @ApiWalletUtxoSnapshot) o
            expectCliField (#entries) (`shouldBe` []) utxoSnap

    it "WALLETS_UTXO_02 - Utxo statistics works properly" $ \ctx -> runResourceT $ do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx

        --send transactions to the wallet
        let coins :: [Natural]
            coins =
                [13_000_000, 43_000_000, 66_000_000, 101_000_000, 1339_000_000]
        addrs:_ <- listAddresses @n ctx wDest
        let addr = encodeAddress @n (getApiT $ fst $ addrs ^. #id)

        let payments = flip map coins $ \c ->
                ["--payment", show c <> "@" <> T.unpack addr ]
        let args = T.unpack (wSrc ^. walletId) : mconcat payments

        (cp, op, ep) <- postTransactionViaCLI ctx "cardano-wallet" args
        T.unpack ep `shouldContain` cmdOk
        _ <- expectValidJSON (Proxy @(ApiTransaction n)) op
        cp `shouldBe` ExitSuccess
        eventually "Wallet balance is as expected" $ do
            Stdout og <- getWalletViaCLI ctx $ T.unpack (wDest ^. walletId)
            jg <- expectValidJSON (Proxy @ApiWallet) og
            expectCliField (#balance . #available)
                (`shouldBe` Quantity (fromIntegral $ sum coins)) jg
            expectCliField (#balance . #total)
                (`shouldBe` Quantity (fromIntegral $ sum coins)) jg

        --verify utxo
        (Exit c, Stdout o, Stderr e) <-
            getWalletUtxoStatisticsViaCLI ctx $ T.unpack (wDest ^. walletId)
        c `shouldBe` ExitSuccess
        e `shouldBe` cmdOk
        utxoStats1 <- expectValidJSON (Proxy @ApiUtxoStatistics) o
        expectWalletUTxO coins (Right utxoStats1)

    describe "WALLETS_UTXO_03 - non-existing wallets" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \ctx -> runResourceT $ do
            (Exit c, Stdout out, Stderr err) <- getWalletUtxoStatisticsViaCLI ctx walId
            out `shouldBe` mempty
            c `shouldBe` ExitFailure 1
            if (title == "40 chars hex") then
                err `shouldContain` errMsg404NoWallet (T.pack $ replicate 40 '1')
            else
                err `shouldContain` errMsg400WalletIdEncoding

    it "WALLETS_UTXO_03 - Deleted wallet is not available for utxo" $ \ctx -> runResourceT $ do
        wid <- emptyWallet' ctx
        Exit cd <- deleteWalletViaCLI ctx wid
        cd `shouldBe` ExitSuccess

        (Exit c, Stdout o, Stderr e) <- getWalletUtxoStatisticsViaCLI ctx wid
        c `shouldBe` ExitFailure 1
        e `shouldContain` errMsg404NoWallet (T.pack wid)
        o `shouldBe` mempty

    it "WALLETS_UTXO_03 - 'almost' valid walletId" $ \ctx -> runResourceT $ do
        wid <- emptyWallet' ctx
        (Exit c, Stdout o, Stderr e)
                <- getWalletUtxoStatisticsViaCLI ctx (wid ++ "1")
        c `shouldBe` ExitFailure 1
        e `shouldContain` errMsg400WalletIdEncoding
        o `shouldBe` mempty

  where
      expect (expEc, expOut, expErr) (ec, out, err) = do
              ec `shouldBe` expEc
              out `shouldBe` expOut
              T.unpack err `shouldContain` expErr

emptyRandomWallet'
    :: (MonadIO m, MonadUnliftIO m)
    => Context
    -> ResourceT m String
emptyRandomWallet' = fmap (T.unpack . view walletId) . emptyRandomWallet

emptyWallet' :: (MonadIO m, MonadUnliftIO m) => Context -> ResourceT m String
emptyWallet' = fmap (T.unpack . view walletId) . emptyWallet

emptyWalletWith'
    :: (MonadIO m, MonadUnliftIO m)
    => Context -> (Text, Text, Int) -> ResourceT m String
emptyWalletWith' ctx (name, pass, pg) =
    fmap (T.unpack . view walletId) (emptyWalletWith ctx (name, pass, pg))

walletNames :: [(String, String)]
walletNames =
        [ ( "Name min", replicate walletNameMinLength 'ź' )
        , ( "Name max", replicate walletNameMaxLength 'ą' )
        , ( "Name max - 1", replicate ( walletNameMaxLength - 1 ) 'ą' )
        , ( "Name min + 1", replicate ( walletNameMinLength + 1 ) 'ź' )
        , ( "Single space", " " )
        , ( "Russian", "АаБбВвГгДдЕеЁёЖжЗз")
        , ( "Polish", "aąbcćdeęfghijklłmnoóp" )
        , ( "Kanji", "亜哀挨愛曖悪握圧扱宛嵐")
        ]

walletNamesInvalid :: [(String, String)]
walletNamesInvalid =
    [ ("", "name is too short: expected at least 1 character")
    , (replicate (walletNameMaxLength + 1) 'ą', "name is too long: expected at most 255 characters")
    ]
