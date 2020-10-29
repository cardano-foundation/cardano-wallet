{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Byron.Wallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiUtxoStatistics
    , ApiWalletDiscovery (..)
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PassphraseMaxLength (..), PassphraseMinLength (..), PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Control.Monad
    ( forM_, void )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Maybe
    ( isJust, isNothing )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( SpecWith, describe, runIO, shouldNotBe, shouldSatisfy )
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , MnemonicLength (..)
    , Payload (..)
    , emptyByronWalletFromXPrvWith
    , emptyByronWalletWith
    , emptyIcarusWallet
    , emptyRandomWallet
    , emptyRandomWalletWithPasswd
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , expectWalletUTxO
    , fixturePassphrase
    , fixturePassphraseEncrypted
    , genMnemonics
    , getFromResponse
    , json
    , request
    , rootPrvKeyFromMnemonics
    , verify
    , walletId
    )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , errMsg400NumberOfWords
    , errMsg403WrongPass
    , errMsg404NoWallet
    , kanjiWalletName
    , polishWalletName
    , russianWalletName
    , updateEmptyPassPayload
    , updatePassPayload
    , wildcardsWalletName
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n t.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n IcarusKey
    , PaymentAddress n ByronKey
    ) => SpecWith (Context t)
spec = describe "BYRON_WALLETS" $ do
    it "BYRON_GET_04, DELETE_01 - Deleted wallet is not available" $ \ctx -> do
        w <- emptyRandomWallet ctx
        _ <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
        rg <- request @ApiByronWallet ctx (Link.getWallet @'Byron w) Default Empty
        expectResponseCode @IO HTTP.status404 rg
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) rg

    it "BYRON_LIST_01 - Byron Wallets are listed from oldest to newest" $
        \ctx -> do
            m1 <- genMnemonics M12
            m2 <- genMnemonics M12
            m3 <- genMnemonics M12
            _ <- emptyByronWalletWith ctx "random" ("b1", m1, fixturePassphrase)
            _ <- emptyByronWalletWith ctx "random" ("b2", m2, fixturePassphrase)
            _ <- emptyByronWalletWith ctx "random" ("b3", m3, fixturePassphrase)

            rl <- request @[ApiByronWallet] ctx (Link.listWallets @'Byron) Default Empty
            verify rl
                [ expectResponseCode @IO HTTP.status200
                , expectListSize 3
                , expectListField 0
                        (#name . #getApiT . #getWalletName) (`shouldBe` "b1")
                , expectListField 1
                        (#name . #getApiT . #getWalletName) (`shouldBe` "b2")
                , expectListField 2
                        (#name . #getApiT . #getWalletName) (`shouldBe` "b3")
                ]

    it "BYRON_LIST_01 - Interleave of Icarus and Random wallets" $ \ctx -> do
        let pwd = fixturePassphrase
        genMnemonics M15 >>= \m -> void (emptyByronWalletWith ctx "icarus" ("ica1", m, pwd))
        genMnemonics M12 >>= \m -> void (emptyByronWalletWith ctx "random" ("rnd2", m, pwd))
        genMnemonics M15 >>= \m -> void (emptyByronWalletWith ctx "icarus" ("ica3", m, pwd))
        rl <- request @[ApiByronWallet] ctx (Link.listWallets @'Byron) Default Empty
        verify rl
            [ expectResponseCode @IO HTTP.status200
            , expectListSize 3
            , expectListField 0
                (#name . #getApiT . #getWalletName) (`shouldBe` "ica1")
            , expectListField 1
                (#name . #getApiT . #getWalletName) (`shouldBe` "rnd2")
            , expectListField 2
                (#name . #getApiT . #getWalletName) (`shouldBe` "ica3")
            ]

    describe "BYRON_RESTORE_01, GET_01, LIST_01 - Restore a wallet" $ do
        let scenarioSuccess style mnemonic ctx = do
                let name = "Empty Byron Wallet"
                let payload = Json [json| {
                        "name": #{name},
                        "mnemonic_sentence": #{mnemonic},
                        "passphrase": #{fixturePassphrase},
                        "style": #{style}
                    }|]
                let discovery =
                        if style == "random"
                        then DiscoveryRandom
                        else DiscoverySequential
                let expectations =
                            [ expectField (#name . #getApiT . #getWalletName) (`shouldBe` name)
                            , expectField (#balance . #available) (`shouldBe` Quantity 0)
                            , expectField (#balance . #total) (`shouldBe` Quantity 0)
                            , expectField #passphrase (`shouldNotBe` Nothing)
                            , expectField #discovery (`shouldBe` discovery)
                            ]
                -- create
                r <- request @ApiByronWallet ctx
                    (Link.postWallet @'Byron) Default payload
                verify r expectations
                let w = getFromResponse id r

                eventually "wallet is available and ready" $ do
                    -- get
                    rg <- request @ApiByronWallet ctx
                        (Link.getWallet @'Byron w) Default Empty
                    verify rg $
                        (expectField (#state . #getApiT) (`shouldBe` Ready)) : expectations
                    -- list
                    rl <- request @[ApiByronWallet] ctx
                        (Link.listWallets @'Byron) Default Empty
                    verify rl
                        [ expectResponseCode @IO HTTP.status200
                        , expectListSize 1
                        , expectListField 0
                                (#name . #getApiT . #getWalletName) (`shouldBe` name)
                        , expectListField 0
                                (#balance . #available) (`shouldBe` Quantity 0)
                        , expectListField 0
                                (#state . #getApiT) (`shouldBe` Ready)
                        , expectListField 0
                                (#balance . #total) (`shouldBe` Quantity 0)
                        ]

        let scenarioFailure style mnemonic ctx = do
                let payload = Json [json| {
                        "name": "Empty Byron Wallet",
                        "mnemonic_sentence": #{mnemonic},
                        "passphrase": #{fixturePassphrase},
                        "style": #{style}
                    }|]
                r <- request @ApiByronWallet ctx
                    (Link.postWallet @'Byron) Default payload
                verify r
                    [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage errMsg400NumberOfWords
                    ]

        let it' style genMnemonicIO test = do
                mnemonic <- runIO genMnemonicIO
                flip it (test style mnemonic) $ unwords
                    [ style
                    , show (length mnemonic)
                    , "words"
                    ]

        it' "random" (genMnemonics M9)  scenarioFailure -- ❌
        it' "random" (genMnemonics M12) scenarioSuccess -- ✔️
        it' "random" (genMnemonics M15) scenarioSuccess -- ✔️
        it' "random" (genMnemonics M18) scenarioSuccess -- ✔️
        it' "random" (genMnemonics M21) scenarioSuccess -- ✔️
        it' "random" (genMnemonics M24) scenarioSuccess -- ✔️

        it' "icarus" (genMnemonics M9)  scenarioFailure -- ❌
        it' "icarus" (genMnemonics M12) scenarioSuccess -- ✔️
        it' "icarus" (genMnemonics M15) scenarioSuccess -- ✔️
        it' "icarus" (genMnemonics M18) scenarioSuccess -- ✔️
        it' "icarus" (genMnemonics M21) scenarioSuccess -- ✔️
        it' "icarus" (genMnemonics M24) scenarioSuccess -- ✔️

        it' "trezor" (genMnemonics M9)  scenarioFailure -- ❌
        it' "trezor" (genMnemonics M12) scenarioSuccess -- ✔️
        it' "trezor" (genMnemonics M15) scenarioSuccess -- ✔️
        it' "trezor" (genMnemonics M18) scenarioSuccess -- ✔️
        it' "trezor" (genMnemonics M21) scenarioSuccess -- ✔️
        it' "trezor" (genMnemonics M24) scenarioSuccess -- ✔️

        it' "ledger" (genMnemonics M9)  scenarioFailure -- ❌
        it' "ledger" (genMnemonics M12) scenarioSuccess -- ✔️
        it' "ledger" (genMnemonics M15) scenarioSuccess -- ✔️
        it' "ledger" (genMnemonics M18) scenarioSuccess -- ✔️
        it' "ledger" (genMnemonics M21) scenarioSuccess -- ✔️
        it' "ledger" (genMnemonics M24) scenarioSuccess -- ✔️

    it "BYRON_RESTORE_02 - One can restore previously deleted wallet" $
        \ctx -> do
            m <- genMnemonics M12
            w <- emptyByronWalletWith ctx "random"
                ("Byron Wallet", m, fixturePassphrase)
            rd <- request
                @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
            expectResponseCode @IO HTTP.status204 rd
            wr <- emptyByronWalletWith ctx "random"
                ("Byron Wallet2", m, "Secure Pa33phrase")
            w ^. walletId `shouldBe` wr ^. walletId

    it "BYRON_RESTORE_03 - Cannot restore wallet that exists" $ \ctx -> do
        mnemonic <- genMnemonics M12
        let payload = Json [json| {
                "name": "Some Byron Wallet",
                "mnemonic_sentence": #{mnemonic},
                "passphrase": #{fixturePassphrase},
                "style": "random"
                } |]
        r1 <- request @ApiByronWallet ctx (Link.postWallet @'Byron) Default payload
        expectResponseCode @IO HTTP.status201 r1

        r2 <- request @ApiByronWallet ctx (Link.postWallet @'Byron) Default payload
        verify r2
            [ expectResponseCode @IO HTTP.status409
            , expectErrorMessage ("This operation would yield a wallet with the\
                 \ following id: " ++ T.unpack (getFromResponse walletId r1) ++
                 " However, I already know of a wallet with this id.")
            ]

    describe "BYRON_RESTORE_06 - Passphrase" $ do
        let minLength = passphraseMinLength (Proxy @"raw")
        let maxLength = passphraseMaxLength (Proxy @"raw")
        let matrix =
                [ ( show minLength ++ " char long"
                  , T.pack (replicate minLength 'ź')
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                , ( show maxLength ++ " char long"
                  , T.pack (replicate maxLength 'ą')
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                , ( "Russian passphrase", russianWalletName
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                , ( "Polish passphrase", polishWalletName
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                , ( "Kanji passphrase", kanjiWalletName
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                , ( "Arabic passphrase", arabicWalletName
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                , ( "Wildcards passphrase", wildcardsWalletName
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                ]
        forM_ matrix $ \(title, passphrase, expectations) -> it title $
            \ctx -> do
                mnemonics12 <- genMnemonics M12
                let payload = Json [json| {
                        "name": "Secure Wallet",
                        "mnemonic_sentence": #{mnemonics12},
                        "passphrase": #{passphrase},
                        "style": "random"
                        } |]
                r <- request
                    @ApiByronWallet ctx (Link.postWallet @'Byron) Default payload
                verify r expectations

    it "BYRON_UPDATE_NAME_01 - Update names of wallets" $ \ctx ->
        forM_ [ (emptyRandomWallet ctx, "Random Wallet")
              , (emptyIcarusWallet ctx, "Icarus Wallet")
              ] $
        \(emptyByronWallet, wName) -> do
            w <- emptyByronWallet
            r1 <- request @ApiByronWallet ctx
                  (Link.getWallet @'Byron w) Default Empty
            verify r1
                [ expectField (#name . #getApiT . #getWalletName) (`shouldBe` wName) ]
            let updatedName = "new wallet 1"
            let payload = Json [json| {
                    "name": #{updatedName}
                    } |]
            r2 <- request @ApiByronWallet ctx
                  (Link.putWallet @'Byron w) Default payload
            verify r2
                [ expectResponseCode @IO HTTP.status200
                , expectField (#name . #getApiT . #getWalletName) (`shouldBe` updatedName)
                ]

    it "BYRON_UPDATE_NAME_02 - Update names of wallets from Xprv" $ \ctx -> do
        -- Wallet from XPRV
        let wName = "Byron Wallet from XPRV"
        mnemonics <- genMnemonics M12
        let rootXPrv = rootPrvKeyFromMnemonics mnemonics fixturePassphrase
        w <- emptyByronWalletFromXPrvWith ctx "random"
            (wName, rootXPrv, fixturePassphraseEncrypted)

        r1 <- request @ApiByronWallet ctx
              (Link.getWallet @'Byron w) Default Empty
        verify r1
            [ expectField (#name . #getApiT . #getWalletName) (`shouldBe` wName) ]

        -- verify you can update name
        let updatedName = "new wallet 1"
        let payload = Json [json| {
                "name": #{updatedName}
                } |]
        r2 <- request @ApiByronWallet ctx
              (Link.putWallet @'Byron w) Default payload
        verify r2
            [ expectResponseCode @IO HTTP.status200
            , expectField (#name . #getApiT . #getWalletName) (`shouldBe` updatedName)
            ]

    it "BYRON_UTXO_01 - Wallet's inactivity is reflected in utxo" $ \ctx ->
        forM_ [ emptyRandomWallet, emptyIcarusWallet ] $ \emptyByronWallet -> do
        w <- emptyByronWallet ctx
        rStat <- request @ApiUtxoStatistics ctx
                 (Link.getUTxOsStatistics @'Byron w) Default Empty
        expectResponseCode @IO HTTP.status200 rStat
        expectWalletUTxO [] (snd rStat)

    it "BYRON_UPDATE_PASS_01 - change passphrase" $ \ctx ->
        forM_ [ emptyRandomWallet, emptyIcarusWallet ] $ \emptyByronWallet -> do
        w <- emptyByronWallet ctx
        request @ApiByronWallet ctx (Link.getWallet @'Byron w) Default Empty
            >>= flip verify [ expectField #passphrase (`shouldSatisfy` isJust) ]

        let payload = updatePassPayload fixturePassphrase "New Secure Passphrase"
        r <- request @ApiByronWallet ctx
            (Link.putWalletPassphrase @'Byron w) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status204
            ]

    it "BYRON_UPDATE_PASS_02 - Old passphrase incorrect" $ \ctx ->
        forM_ [ emptyRandomWallet, emptyIcarusWallet ] $ \emptyByronWallet -> do
        w <- emptyByronWallet ctx
        request @ApiByronWallet ctx (Link.getWallet @'Byron w) Default Empty
            >>= flip verify [ expectField #passphrase (`shouldSatisfy` isJust) ]
        let payload = updatePassPayload "incorrect-passphrase" "whatever-pass"
        r <- request @ApiByronWallet ctx
            (Link.putWalletPassphrase @'Byron w) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    it "BYRON_UPDATE_PASS_03 - Updating passphrase with no password wallets" $ \ctx -> do
        w <- emptyRandomWalletWithPasswd ctx ""
        request @ApiByronWallet ctx (Link.getWallet @'Byron w) Default Empty
            >>= flip verify [ expectField #passphrase (`shouldSatisfy` isNothing) ]
        let payload = updateEmptyPassPayload "correct-password"
        r <- request @ApiByronWallet ctx
            (Link.putWalletPassphrase @'Byron w) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status204
            ]

    it "BYRON_UPDATE_PASS_04a - Updating passphrase with no password wallets" $ \ctx -> do
        w <- emptyRandomWalletWithPasswd ctx ""
        request @ApiByronWallet ctx (Link.getWallet @'Byron w) Default Empty
            >>= flip verify [ expectField #passphrase (`shouldSatisfy` isNothing) ]
        let payload = updatePassPayload "" "correct-password"
        r <- request @ApiByronWallet ctx
            (Link.putWalletPassphrase @'Byron w) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status204
            ]

    it "BYRON_UPDATE_PASS_04b - Regression test" $ \ctx -> do
        let key = "38e8de9c583441213fe34eecc4e28265267466877ba4048e3ab1fa99563\
                  \66947aefaf5ba9779db67eead7fc9cd1354b994a5d8d9cd40ab874bfeb1\
                  \b33649280cd33651377731e0e59e0233425a55257782c5adaa768da0567\
                  \f43c1c6c0c18766ed0a547bb34eb472c120b170a8640279832ddf180028\
                  \87f03c15dea59705422d"
        let pwd = "31347c387c317c574342652b796362417576356c2b4258676a344a314c6\
                  \343675375414c2f5653393661364e576a2b7550766655513d3d7c6f7846\
                  \36654939734151444e6f38395147747366324e653937426338372b484b6\
                  \b4137756772752f5970673d"
        w <- emptyByronWalletFromXPrvWith ctx "random" ("Random Wallet", key, pwd)
        let payload = updatePassPayload "" "correct-password"
        r <- request @ApiByronWallet ctx
            (Link.putWalletPassphrase @'Byron w) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status204
            ]

    it "BYRON_UPDATE_PASS_07 - Updating passphrase with short password wallets" $ \ctx -> do
        w <- emptyRandomWalletWithPasswd ctx "cos"
        request @ApiByronWallet ctx (Link.getWallet @'Byron w) Default Empty
            >>= flip verify [ expectField #passphrase (`shouldSatisfy` isJust) ]
        let payload = updatePassPayload "cos" "correct-password"
        r <- request @ApiByronWallet ctx
            (Link.putWalletPassphrase @'Byron w) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status204
            ]
