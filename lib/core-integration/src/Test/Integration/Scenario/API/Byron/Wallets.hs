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

import Cardano.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , MnemonicWords
    , ValidChecksumSize
    , ValidEntropySize
    , entropyToMnemonic
    , genEntropy
    , mnemonicToText
    )
import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiUtxoStatistics
    , ApiWalletDiscovery (..)
    , ApiWalletMigrationInfo (..)
    , DecodeAddress
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PassphraseMaxLength (..), PassphraseMinLength (..), PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.Types
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
import Data.Text
    ( Text )
import Test.Hspec
    ( SpecWith, describe, it, runIO, shouldNotBe, shouldSatisfy )
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
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
    , faucetAmt
    , fixtureIcarusWallet
    , fixturePassphrase
    , fixturePassphraseEncrypted
    , fixtureRandomWallet
    , getFromResponse
    , json
    , request
    , rootPrvKeyFromMnemonics
    , unsafeRequest
    , verify
    , walletId
    , (.>)
    )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , errMsg400NumberOfWords
    , errMsg403NothingToMigrate
    , errMsg403WrongPass
    , errMsg404NoWallet
    , kanjiWalletName
    , mnemonics12
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
    , EncodeAddress n
    , PaymentAddress n ByronKey
    ) => SpecWith (Context t)
spec = do
    it "BYRON_CALCULATE_01 - \
        \for non-empty wallet calculated fee is > zero."
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> do
            w <- fixtureByronWallet ctx
            let ep = Link.getMigrationInfo w
            r <- request @ApiWalletMigrationInfo ctx ep Default Empty
            verify r
                [ expectResponseCode @IO HTTP.status200
                , expectField (#migrationCost . #getQuantity)
                    (.> 0)
                ]

    it "BYRON_CALCULATE_02 - \
        \Cannot calculate fee for empty wallet."
        $ \ctx -> forM_ [emptyRandomWallet, emptyIcarusWallet] $ \emptyByronWallet -> do
            w <- emptyByronWallet ctx
            let ep = Link.getMigrationInfo w
            r <- request @ApiWalletMigrationInfo ctx ep Default Empty
            verify r
                [ expectResponseCode @IO HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate $ w ^. walletId)
                ]

    it "BYRON_CALCULATE_02 - \
        \Cannot calculate fee for wallet with dust, that cannot be migrated."
        $ \ctx -> do
            -- NOTE
            -- Special mnemonic for which wallet with dust
            -- (1 utxo with 10 lovelace)
            let mnemonics =
                    [ "prison", "census", "discover", "give"
                    , "sound", "behave", "hundred", "cave"
                    , "someone", "orchard", "just", "wild"
                    ] :: [Text]
            let payloadRestore = Json [json| {
                    "name": "Dust Byron Wallet",
                    "mnemonic_sentence": #{mnemonics},
                    "passphrase": #{fixturePassphrase},
                    "style": "random"
                    } |]
            (_, w) <- unsafeRequest @ApiByronWallet ctx
                (Link.postWallet @'Byron) payloadRestore
            let ep = Link.getMigrationInfo w
            r <- request @ApiWalletMigrationInfo ctx ep Default Empty
            verify r
                [ expectResponseCode @IO HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate $ w ^. walletId)
                ]

    it "BYRON_GET_04, DELETE_01 - Deleted wallet is not available" $ \ctx -> do
        w <- emptyRandomWallet ctx
        _ <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
        rg <- request @ApiByronWallet ctx (Link.getWallet @'Byron w) Default Empty
        expectResponseCode @IO HTTP.status404 rg
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) rg

    it "BYRON_LIST_01 - Byron Wallets are listed from oldest to newest" $
        \ctx -> do
            m1 <- genMnemonics @12
            m2 <- genMnemonics @12
            m3 <- genMnemonics @12
            _ <- emptyByronWalletWith ctx "random" ("b1", m1, "Secure Passphrase")
            _ <- emptyByronWalletWith ctx "random" ("b2", m2, "Secure Passphrase")
            _ <- emptyByronWalletWith ctx "random" ("b3", m3, "Secure Passphrase")

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
        let pwd = "Secure Passphrase"
        genMnemonics @15 >>= \m -> void (emptyByronWalletWith ctx "icarus" ("ica1", m, pwd))
        genMnemonics @12 >>= \m -> void (emptyByronWalletWith ctx "random" ("rnd2", m, pwd))
        genMnemonics @15 >>= \m -> void (emptyByronWalletWith ctx "icarus" ("ica3", m, pwd))
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
                        "passphrase": "Secure Passphrase",
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
                        "passphrase": "Secure Passphrase",
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

        it' "random" (genMnemonics @9)  scenarioFailure -- ❌
        it' "random" (genMnemonics @12) scenarioSuccess -- ✔️
        it' "random" (genMnemonics @15) scenarioFailure -- ❌
        it' "random" (genMnemonics @18) scenarioFailure -- ❌
        it' "random" (genMnemonics @21) scenarioFailure -- ❌
        it' "random" (genMnemonics @24) scenarioFailure -- ❌

        it' "icarus" (genMnemonics @9)  scenarioFailure -- ❌
        it' "icarus" (genMnemonics @12) scenarioFailure -- ❌
        it' "icarus" (genMnemonics @15) scenarioSuccess -- ✔️
        it' "icarus" (genMnemonics @18) scenarioFailure -- ❌
        it' "icarus" (genMnemonics @21) scenarioFailure -- ❌
        it' "icarus" (genMnemonics @24) scenarioFailure -- ❌

        it' "trezor" (genMnemonics @9)  scenarioFailure -- ❌
        it' "trezor" (genMnemonics @12) scenarioSuccess -- ✔️
        it' "trezor" (genMnemonics @15) scenarioSuccess -- ✔️
        it' "trezor" (genMnemonics @18) scenarioSuccess -- ✔️
        it' "trezor" (genMnemonics @21) scenarioSuccess -- ✔️
        it' "trezor" (genMnemonics @24) scenarioSuccess -- ✔️

        it' "ledger" (genMnemonics @9)  scenarioFailure -- ❌
        it' "ledger" (genMnemonics @12) scenarioSuccess -- ✔️
        it' "ledger" (genMnemonics @15) scenarioSuccess -- ✔️
        it' "ledger" (genMnemonics @18) scenarioSuccess -- ✔️
        it' "ledger" (genMnemonics @21) scenarioSuccess -- ✔️
        it' "ledger" (genMnemonics @24) scenarioSuccess -- ✔️

    it "BYRON_RESTORE_02 - One can restore previously deleted wallet" $
        \ctx -> do
            m <- genMnemonics @12
            w <- emptyByronWalletWith ctx "random"
                ("Byron Wallet", m, "Secure Passphrase")
            rd <- request
                @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
            expectResponseCode @IO HTTP.status204 rd
            wr <- emptyByronWalletWith ctx "random"
                ("Byron Wallet2", m, "Secure Pa33phrase")
            w ^. walletId `shouldBe` wr ^. walletId

    it "BYRON_RESTORE_03 - Cannot restore wallet that exists" $ \ctx -> do
        mnemonic <- genMnemonics @12
        let payload = Json [json| {
                "name": "Some Byron Wallet",
                "mnemonic_sentence": #{mnemonic},
                "passphrase": "Secure Passphrase",
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
                let payload = Json [json| {
                        "name": "Secure Wallet",
                        "mnemonic_sentence": #{mnemonics12},
                        "passphrase": #{passphrase},
                        "style": "random"
                        } |]
                r <- request
                    @ApiByronWallet ctx (Link.postWallet @'Byron) Default payload
                verify r expectations

    it "BYRON_RESTORE_08 - Icarus wallet with high indexes" $ \ctx -> do
        -- NOTE
        -- Special Icarus mnemonic where address indexes are all after the index
        -- 500. Because we don't have the whole history, restoring sequential
        -- wallets like Icarus ones is tricky from just a snapshot and we need
        -- to use arbitrarily big address pool gaps.
        let mnemonics =
                [ "erosion", "ahead", "vibrant", "air", "day"
                , "timber", "thunder", "general", "dice", "into"
                , "chest", "enrich", "social", "neck", "shine"
                ] :: [Text]
        let payload = Json [json| {
                "name": "High Index Wallet",
                "mnemonic_sentence": #{mnemonics},
                "passphrase": #{fixturePassphrase},
                "style": "icarus"
                } |]

        r <- request @ApiByronWallet ctx (Link.postWallet @'Byron) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status201
            , expectField (#balance . #available) (`shouldBe` Quantity faucetAmt)
            ]

    it "BYRON_RESTORE_09 - Ledger wallet" $ \ctx -> do
        -- NOTE
        -- Special legacy wallets where addresses have been generated from a
        -- seed derived using the auxiliary method used by Ledger.
        let mnemonics =
                [ "vague" , "wrist" , "poet" , "crazy" , "danger" , "dinner"
                , "grace" , "home" , "naive" , "unfold" , "april" , "exile"
                , "relief" , "rifle" , "ranch" , "tone" , "betray" , "wrong"
                ] :: [Text]
        let payload = Json [json| {
                "name": "Ledger Wallet",
                "mnemonic_sentence": #{mnemonics},
                "passphrase": #{fixturePassphrase},
                "style": "ledger"
                } |]

        r <- request @ApiByronWallet ctx (Link.postWallet @'Byron) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status201
            , expectField (#balance . #available) (`shouldBe` Quantity faucetAmt)
            ]

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
        mnemonics <- genMnemonics @12
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

        let payload = updatePassPayload "Secure Passphrase" "New Secure Passphrase"
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

  where
    genMnemonics
        :: forall mw ent csz.
            ( ConsistentEntropy ent mw csz
            , ValidEntropySize ent
            , ValidChecksumSize ent csz
            , ent ~ EntropySize mw
            , mw ~ MnemonicWords ent
            )
        => IO [Text]
    genMnemonics = mnemonicToText . entropyToMnemonic @mw <$> genEntropy
