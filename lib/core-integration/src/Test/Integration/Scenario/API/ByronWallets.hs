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

module Test.Integration.Scenario.API.ByronWallets
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiByronWalletMigrationInfo (..)
    , ApiTransaction
    , ApiUtxoStatistics
    , ApiWallet
    , ByronWalletStyle (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    )
import Cardano.Wallet.Primitive.Mnemonic
    ( ConsistentEntropy
    , EntropySize
    , MnemonicWords
    , ValidChecksumSize
    , ValidEntropySize
    , entropyToMnemonic
    , genEntropy
    , mnemonicToText
    )
import Cardano.Wallet.Primitive.Types
    ( SyncProgress (..), walletNameMaxLength, walletNameMinLength )
import Control.Monad
    ( forM_, void )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import Test.Hspec
    ( SpecWith, describe, it, runIO, shouldNotBe, shouldSatisfy )
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , RequestException (..)
    , emptyByronWalletWith
    , emptyIcarusWallet
    , emptyRandomWallet
    , emptyWallet
    , emptyWalletWith
    , eventually_
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , faucetAmt
    , fixtureIcarusWallet
    , fixturePassphrase
    , fixtureRandomWallet
    , fixtureWallet
    , getFromResponse
    , greaterThan
    , json
    , request
    , unsafeRequest
    , verify
    , walletId
    , withMethod
    , withPathParam
    )
import Test.Integration.Framework.TestData
    ( arabicWalletName
    , errMsg400NumberOfWords
    , errMsg400ParseError
    , errMsg403NothingToMigrate
    , errMsg403WrongPass
    , errMsg404NoEndpoint
    , errMsg404NoWallet
    , errMsg405
    , errMsgNotInDictionary
    , falseWalletIds
    , frenchMnemonics12
    , getHeaderCases
    , invalidMnemonics12
    , japaneseMnemonics12
    , kanjiWalletName
    , mnemonics12
    , polishWalletName
    , postHeaderCases
    , russianWalletName
    , specMnemonicByron
    , wildcardsWalletName
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Api.Types as ApiTypes
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t n. (n ~ 'Testnet) => SpecWith (Context t)
spec = do

    -- Compute the fee associated with an API transaction.
    let apiTransactionFee :: ApiTransaction n -> Word64
        apiTransactionFee t =
            inputBalance t - outputBalance t
          where
            inputBalance = fromIntegral
                . sum
                . fmap (view (#amount . #getQuantity))
                . mapMaybe ApiTypes.source
                . view #inputs
            outputBalance = fromIntegral
                . sum
                . fmap (view (#amount . #getQuantity))
                . view #outputs

    it "BYRON_CALCULATE_01 - \
        \for non-empty wallet calculated fee is > zero."
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet] $ \fixtureByronWallet -> do
            w <- fixtureByronWallet ctx
            let ep = Link.getMigrationInfo w
            r <- request @ApiByronWalletMigrationInfo ctx ep Default Empty
            verify r
                [ expectResponseCode @IO HTTP.status200
                , expectField (#migrationCost . #getQuantity)
                    (greaterThan 0)
                ]

    it "BYRON_CALCULATE_02 - \
        \Cannot calculate fee for empty wallet."
        $ \ctx -> forM_ [emptyRandomWallet, emptyIcarusWallet] $ \emptyByronWallet -> do
            w <- emptyByronWallet ctx
            let ep = Link.getMigrationInfo w
            r <- request @ApiByronWalletMigrationInfo ctx ep Default Empty
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
                    "passphrase": #{fixturePassphrase}
                    } |]
            (_, w) <- unsafeRequest @ApiByronWallet ctx
                (Link.postWallet @'Random) payloadRestore
            let ep = Link.getMigrationInfo w
            r <- request @ApiByronWalletMigrationInfo ctx ep Default Empty
            verify r
                [ expectResponseCode @IO HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate $ w ^. walletId)
                ]

    it "BYRON_CALCULATE_03 - \
        \Cannot estimate migration for Shelley wallet"
        $ \ctx -> do
            w <- emptyWallet ctx
            let ep = Link.getMigrationInfo w
            r <- request @ApiByronWalletMigrationInfo ctx ep Default Empty
            expectResponseCode @IO HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "BYRON_CALCULATE_04 - non-existing wallets" $ do
        forM_ falseWalletIds $ \(desc, walId) -> do
            it desc $ \ctx -> do
                w <- emptyRandomWallet ctx
                let endpoint = withPathParam 0 (const $ T.pack walId) $
                        Link.getWallet @'Byron w
                rg <- request @ApiByronWallet ctx endpoint Default Empty
                expectResponseCode @IO HTTP.status404 rg
                if (desc == "40 chars hex") then
                    expectErrorMessage (errMsg404NoWallet $ T.pack walId) rg
                else
                    expectErrorMessage errMsg404NoEndpoint rg

    describe "BYRON_CALCULATE_05 - HTTP headers" $ do
        forM_ (getHeaderCases HTTP.status200)
            $ \(title, headers, expectations) -> it title $ \ctx -> do
            w <- fixtureRandomWallet ctx
            let ep = Link.getMigrationInfo w
            r <- request @ApiByronWalletMigrationInfo ctx ep headers Empty
            verify r expectations

    it "BYRON_MIGRATE_01 - \
        \after a migration operation successfully completes, the correct \
        \amount eventually becomes available in the target wallet."
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet] $ \fixtureByronWallet -> do
            -- Restore a Byron wallet with funds, to act as a source wallet:
            sourceWallet <- fixtureByronWallet ctx
            let originalBalance =
                        view (#balance . #available . #getQuantity) sourceWallet

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx

            -- Calculate the expected migration fee:
            r0 <- request @ApiByronWalletMigrationInfo ctx
                (Link.getMigrationInfo sourceWallet) Default Empty
            verify r0
                [ expectResponseCode @IO HTTP.status200
                , expectField #migrationCost (greaterThan $ Quantity 0)
                ]
            let expectedFee = getFromResponse (#migrationCost . #getQuantity) r0

            -- Perform a migration from the source wallet to the target wallet:
            r1 <- request @[ApiTransaction n] ctx
                (Link.migrateWallet sourceWallet targetWallet)
                Default
                (Json [aesonQQ|{"passphrase": #{fixturePassphrase}}|])
            verify r1
                [ expectResponseCode @IO HTTP.status202
                , expectField id (`shouldSatisfy` (not . null))
                ]

            -- Check that funds become available in the target wallet:
            let expectedBalance = originalBalance - expectedFee
            eventually_ $ do
                r2 <- request @ApiWallet ctx
                    (Link.getWallet @'Shelley targetWallet) Default Empty
                verify r2
                    [ expectField
                            (#balance . #getApiT . #available)
                            (`shouldBe` Quantity expectedBalance)
                    , expectField
                            (#balance . #getApiT . #total)
                            (`shouldBe` Quantity expectedBalance)
                    ]

    it "BYRON_MIGRATE_01 - \
        \ migrate a big wallet requiring more than one tx" $ \ctx -> do
        -- NOTE
        -- Special mnemonic for which 500 legacy funds are attached to in the
        -- genesis file.
        --
        -- Out of these 500 coins, 100 of them are of 1 Lovelace and are
        -- expected to be treated as dust. The rest are all worth:
        -- 10,000,000,000 lovelace.
        let mnemonics =
                ["collect", "fold", "file", "clown"
                , "injury", "sun", "brass", "diet"
                , "exist", "spike", "behave", "clip"
                ] :: [Text]
        let payloadRestore = Json [json| {
                "name": "Big Byron Wallet",
                "mnemonic_sentence": #{mnemonics},
                "passphrase": #{fixturePassphrase}
                } |]
        (_, wOld) <- unsafeRequest @ApiByronWallet ctx
            (Link.postWallet @'Random) payloadRestore
        eventually_ $ do
            request @ApiByronWallet ctx
                (Link.getWallet @'Byron wOld)
                Default
                Empty >>= flip verify
                [ expectField (#balance . #available) (greaterThan $ Quantity 0)
                ]
        let originalBalance = view (#balance . #available . #getQuantity) wOld

        -- Calculate the expected migration fee:
        rFee <- request @ApiByronWalletMigrationInfo ctx
            (Link.getMigrationInfo wOld)
            Default
            Empty
        verify rFee
            [ expectResponseCode @IO HTTP.status200
            , expectField #migrationCost (greaterThan $ Quantity 0)
            ]
        let expectedFee = getFromResponse (#migrationCost . #getQuantity) rFee

        -- Migrate to a new empty wallet
        wNew <- emptyWallet ctx
        let payloadMigrate = Json [json|{"passphrase": #{fixturePassphrase}}|]
        request @[ApiTransaction n] ctx
            (Link.migrateWallet wOld wNew)
            Default
            payloadMigrate >>= flip verify
            [ expectResponseCode @IO HTTP.status202
            , expectField id ((`shouldBe` 2). length)
            ]

        -- Check that funds become available in the target wallet:
        let expectedBalance = originalBalance - expectedFee
        eventually_ $ do
            request @ApiWallet ctx
                (Link.getWallet @'Shelley wNew)
                Default
                Empty >>= flip verify
                [ expectField
                        (#balance . #getApiT . #available)
                        ( `shouldBe` Quantity expectedBalance)
                , expectField
                        (#balance . #getApiT . #total)
                        ( `shouldBe` Quantity expectedBalance)
                ]

        -- Analyze the target wallet UTxO distribution
        request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics wNew)
            Default
            Empty >>= flip verify
            [ expectField
                #distribution
                ((`shouldBe` (Just 400)) . Map.lookup 10_000_000_000)
            ]

    it "BYRON_MIGRATE_01 - \
        \a migration operation removes all funds from the source wallet."
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet] $ \fixtureByronWallet -> do
            -- Restore a Byron wallet with funds, to act as a source wallet:
            sourceWallet <- fixtureByronWallet ctx

            -- Perform a migration from the source wallet to a target wallet:
            targetWallet <- emptyWallet ctx
            r0 <- request @[ApiTransaction n] ctx
                (Link.migrateWallet sourceWallet targetWallet )
                Default
                (Json [json|{"passphrase": #{fixturePassphrase}}|])
            verify r0
                [ expectResponseCode @IO HTTP.status202
                , expectField id (`shouldSatisfy` (not . null))
                ]

            -- Verify that the source wallet has no funds available:
            r1 <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron sourceWallet) Default Empty
            verify r1
                [ expectResponseCode @IO HTTP.status200
                , expectField (#balance . #available) (`shouldBe` Quantity 0)
                ]

    it "BYRON_MIGRATE_02 - \
        \migrating an empty wallet should fail."
        $ \ctx -> forM_ [emptyRandomWallet, emptyIcarusWallet] $ \emptyByronWallet -> do
            sourceWallet <- emptyByronWallet ctx
            targetWallet <- emptyWallet ctx
            let payload = Json [json|{"passphrase": "Secure Passphrase"}|]
            let ep = Link.migrateWallet sourceWallet targetWallet
            r <- request @[ApiTransaction n] ctx ep Default payload
            let srcId = sourceWallet ^. walletId
            verify r
                [ expectResponseCode @IO HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate srcId)
                ]

    it "BYRON_MIGRATE_02 - \
        \migrating wallet with dust should fail."
        $ \ctx -> do
            -- NOTE
            -- Special mnemonic for which wallet with dust
            -- (5 utxos with 60 lovelace in total)
            let mnemonics =
                    [ "suffer", "decorate", "head", "opera"
                    , "yellow", "debate", "visa", "fire"
                    , "salute", "hybrid", "stone", "smart"
                    ] :: [Text]
            let payloadRestore = Json [json| {
                    "name": "Dust Byron Wallet",
                    "mnemonic_sentence": #{mnemonics},
                    "passphrase": #{fixturePassphrase}
                    } |]
            (_, sourceWallet) <- unsafeRequest @ApiByronWallet ctx
                (Link.postWallet @'Random) payloadRestore
            eventually_ $ do
                request @ApiByronWallet ctx
                    (Link.getWallet @'Byron sourceWallet)
                    Default
                    Empty >>= flip verify
                    [ expectField (#balance . #available) (greaterThan $ Quantity 0)
                    ]

            targetWallet <- emptyWallet ctx
            let payload = Json [json|{"passphrase": #{fixturePassphrase}}|]
            let ep = Link.migrateWallet sourceWallet targetWallet
            r <- request @[ApiTransaction n] ctx ep Default payload
            let srcId = sourceWallet ^. walletId
            verify r
                [ expectResponseCode @IO HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate srcId)
                ]

    it "BYRON_MIGRATE_03 - \
        \actual fee for migration is the same as the predicted fee."
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet] $ \fixtureByronWallet -> do
            -- Restore a Byron wallet with funds.
            sourceWallet <- fixtureByronWallet ctx

            -- Request a migration fee prediction.
            let ep0 = (Link.getMigrationInfo sourceWallet)
            r0 <- request @ApiByronWalletMigrationInfo ctx ep0 Default Empty
            verify r0
                [ expectResponseCode @IO HTTP.status200
                , expectField #migrationCost (greaterThan $ Quantity 0)
                ]

            -- Perform the migration.
            targetWallet <- emptyWallet ctx
            let payload = Json [json|{"passphrase": #{fixturePassphrase}}|]
            let ep1 = Link.migrateWallet sourceWallet targetWallet
            r1 <- request @[ApiTransaction n] ctx ep1 Default payload
            verify r1
                [ expectResponseCode @IO HTTP.status202
                , expectField id (`shouldSatisfy` (not . null))
                ]

            -- Verify that the fee prediction was correct.
            let actualFee = fromIntegral $ sum $ apiTransactionFee
                    <$> getFromResponse id r1
            let predictedFee =
                    getFromResponse (#migrationCost . #getQuantity) r0
            actualFee `shouldBe` predictedFee

    it "BYRON_MIGRATE_04 - migration fails with a wrong passphrase"
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet] $ \fixtureByronWallet -> do
        -- Restore a Byron wallet with funds, to act as a source wallet:
        sourceWallet <- fixtureByronWallet ctx

        -- Perform a migration from the source wallet to a target wallet:
        targetWallet <- emptyWallet ctx
        r0 <- request @[ApiTransaction n] ctx
            (Link.migrateWallet sourceWallet targetWallet )
            Default
            (Json [json|{"passphrase": "not-the-right-passphrase"}|])
        verify r0
            [ expectResponseCode @IO HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    describe "BYRON_MIGRATE_05 -\
        \ migrating from/to inappropriate wallet types" $ do

        it "Byron -> Byron" $ \ctx -> do
            sWallet <- fixtureRandomWallet ctx
            tWallet <- emptyRandomWallet ctx

            r <- request @[ApiTransaction n] ctx
                (Link.migrateWallet sWallet tWallet )
                Default
                (Json [json|{ "passphrase": #{fixturePassphrase} }|])
            verify r
                [ expectResponseCode @IO HTTP.status404
                , expectErrorMessage (errMsg404NoWallet $ tWallet ^. walletId)
                ]

        it "Icarus -> Icarus" $ \ctx -> do
            sWallet <- fixtureIcarusWallet ctx
            tWallet <- emptyIcarusWallet ctx

            r <- request @[ApiTransaction n] ctx
                (Link.migrateWallet sWallet tWallet )
                Default
                (Json [json|{ "passphrase": #{fixturePassphrase} }|])
            verify r
                [ expectResponseCode @IO HTTP.status404
                , expectErrorMessage (errMsg404NoWallet $ tWallet ^. walletId)
                ]

        it "Icarus -> Byron" $ \ctx -> do
            sWallet <- fixtureIcarusWallet ctx
            tWallet <- emptyRandomWallet ctx

            r <- request @[ApiTransaction n] ctx
                (Link.migrateWallet sWallet tWallet )
                Default
                (Json [json|{ "passphrase": #{fixturePassphrase} }|])
            verify r
                [ expectResponseCode @IO HTTP.status404
                , expectErrorMessage (errMsg404NoWallet $ tWallet ^. walletId)
                ]

        it "Byron -> Icarus" $ \ctx -> do
            sWallet <- fixtureRandomWallet ctx
            tWallet <- emptyIcarusWallet ctx

            r <- request @[ApiTransaction n] ctx
                (Link.migrateWallet sWallet tWallet )
                Default
                (Json [json|{ "passphrase": #{fixturePassphrase} }|])
            verify r
                [ expectResponseCode @IO HTTP.status404
                , expectErrorMessage (errMsg404NoWallet $ tWallet ^. walletId)
                ]

        it "Shelley -> Byron" $ \ctx -> do
            sWallet <- fixtureWallet ctx
            tWallet <- emptyRandomWallet ctx

            r <- request @[ApiTransaction n] ctx
                (Link.migrateWallet sWallet tWallet )
                Default
                (Json [json|{ "passphrase": #{fixturePassphrase} }|])
            verify r
                [ expectResponseCode @IO HTTP.status404
                , expectErrorMessage (errMsg404NoWallet $ sWallet ^. walletId)
                ]

        it "Shelley -> Icarus" $ \ctx -> do
            sWallet <- fixtureWallet ctx
            tWallet <- emptyIcarusWallet ctx

            r <- request @[ApiTransaction n] ctx
                (Link.migrateWallet sWallet tWallet )
                Default
                (Json [json|{ "passphrase": #{fixturePassphrase} }|])
            verify r
                [ expectResponseCode @IO HTTP.status404
                , expectErrorMessage (errMsg404NoWallet $ sWallet ^. walletId)
                ]

        it "Shelley -> Shelley" $ \ctx -> do
            sWallet <- fixtureWallet ctx
            tWallet <- emptyWallet ctx

            r <- request @[ApiTransaction n] ctx
                (Link.migrateWallet sWallet tWallet )
                Default
                (Json [json|{ "passphrase": #{fixturePassphrase} }|])
            verify r
                [ expectResponseCode @IO HTTP.status404
                , expectErrorMessage (errMsg404NoWallet $ sWallet ^. walletId)
                ]

    describe "BYRON_MIGRATE_06 - non-existing wallets" $  do
        let testMigrationTo
                :: Context t
                -> Text
                -> IO (HTTP.Status, Either RequestException ApiByronWallet)
            testMigrationTo ctx walId = do
                w <- fixtureRandomWallet ctx
                let endpoint =
                        withPathParam 1 (const walId) $ Link.migrateWallet w w
                let payload =
                        Json [json|{ "passphrase": #{fixturePassphrase} }|]
                request @ApiByronWallet ctx endpoint Default payload

        let testMigrationFrom
                :: Context t
                -> Text
                -> IO (HTTP.Status, Either RequestException ApiByronWallet)
            testMigrationFrom ctx walId = do
                w <- emptyWallet ctx
                let endpoint =
                        withPathParam 0 (const walId) $ Link.migrateWallet w w
                let payload =
                        Json [json|{ "passphrase": #{fixturePassphrase} }|]
                request @ApiByronWallet ctx endpoint Default payload

        forM_ falseWalletIds $ \(desc, walId) -> do
            it ("testMigrationTo: " ++ desc) $ \ctx -> do
                rg <- testMigrationTo ctx (T.pack walId)
                expectResponseCode @IO HTTP.status404 rg
                if (desc == "40 chars hex") then
                    expectErrorMessage (errMsg404NoWallet $ T.pack walId) rg
                else
                    expectErrorMessage errMsg404NoEndpoint rg

            it ("testMigrationFrom: " ++ desc) $ \ctx -> do
                rg <- testMigrationFrom ctx (T.pack walId)
                expectResponseCode @IO HTTP.status404 rg
                if (desc == "40 chars hex") then
                    expectErrorMessage (errMsg404NoWallet $ T.pack walId) rg
                else
                    expectErrorMessage errMsg404NoEndpoint rg

    describe "BYRON_MIGRATE_07 - migration - HTTP headers" $ do
        forM_ postHeaderCases $ \(title, headers, expec) ->
            it title $ \ctx -> do
                sourceWallet <- emptyRandomWallet ctx
                targetWallet <- emptyWallet ctx

                r <- request @[ApiTransaction n] ctx
                     (Link.migrateWallet sourceWallet targetWallet )
                     headers
                     (Json [json|{ "passphrase": "Secure Passphrase" }|])
                verify r expec

    it "BYRON_MIGRATE_07 - invalid payload, parser error" $ \ctx -> do
        sourceWallet <- emptyRandomWallet ctx
        targetWallet <- emptyWallet ctx

        r <- request @[ApiTransaction n] ctx
            (Link.migrateWallet sourceWallet targetWallet )
            Default
            (NonJson "{passphrase:,}")
        expectResponseCode @IO HTTP.status400 r
        expectErrorMessage errMsg400ParseError r

    it "BYRON_GET_02 - Byron ep does not show Shelley wallet" $ \ctx -> do
        w <- emptyWallet ctx
        r <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron w) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "BYRON_GET_03 - Shelley ep does not show Byron wallet" $ \ctx -> do
        w <- emptyRandomWallet ctx
        r <- request @ApiWallet ctx
            (Link.getWallet @'Shelley w) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "BYRON_GET_04, DELETE_01 - Deleted wallet is not available" $ \ctx -> do
        w <- emptyRandomWallet ctx
        _ <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
        rg <- request @ApiByronWallet ctx (Link.getWallet @'Byron w) Default Empty
        expectResponseCode @IO HTTP.status404 rg
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) rg

    describe "BYRON_GET_05,06 - non-existing wallets" $  do
        forM_ falseWalletIds $ \(desc, walId) -> it desc $ \ctx -> do
            w <- emptyRandomWallet ctx
            let endpoint = withPathParam 0 (const $ T.pack walId) $
                    Link.getWallet @'Byron w
            rg <- request @ApiByronWallet ctx endpoint Default Empty
            expectResponseCode @IO HTTP.status404 rg
            if (desc == valid40CharHexDesc) then do
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) rg
            else
                expectErrorMessage errMsg404NoEndpoint rg

    describe "BYRON_GET_07 - getByronWallet - Methods Not Allowed" $ do
        let matrix = ["PUT", "POST", "CONNECT", "TRACE", "OPTIONS"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            w <- emptyRandomWallet ctx
            let ep = withMethod method $ Link.getWallet @'Byron w
            r <- request @ApiByronWallet ctx ep Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    describe "BYRON_GET_07 - getByronWallet - HTTP headers" $ do
        forM_ (getHeaderCases HTTP.status200)
            $ \(title, headers, expec) -> it title $ \ctx -> do
            w <- emptyRandomWallet ctx
            rg <- request @ApiWallet ctx (Link.getWallet @'Byron w) headers Empty
            verify rg expec

    it "BYRON_LIST_01 - Byron Wallets are listed from oldest to newest" $
        \ctx -> do
            m1 <- genMnemonics @12
            m2 <- genMnemonics @12
            m3 <- genMnemonics @12
            _ <- emptyByronWalletWith @'Random ctx ("b1", m1, "Secure Passphrase")
            _ <- emptyByronWalletWith @'Random ctx ("b2", m2, "Secure Passphrase")
            _ <- emptyByronWalletWith @'Random ctx ("b3", m3, "Secure Passphrase")

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
        genMnemonics @15 >>= \m -> void (emptyByronWalletWith @'Icarus ctx ("ica1", m, pwd))
        genMnemonics @12 >>= \m -> void (emptyByronWalletWith @'Random ctx ("rnd2", m, pwd))
        genMnemonics @15 >>= \m -> void (emptyByronWalletWith @'Icarus ctx ("ica3", m, pwd))
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

    it "BYRON_LIST_02,03 -\
        \ Byron wallets listed only via Byron endpoints \\\
        \ Shelley wallets listed only via new endpoints" $ \ctx -> do
        m1 <- genMnemonics @12
        m2 <- genMnemonics @12
        m3 <- genMnemonics @12
        _ <- emptyByronWalletWith @'Random ctx ("byron1", m1, "Secure Passphrase")
        _ <- emptyByronWalletWith @'Random ctx ("byron2", m2, "Secure Passphrase")
        _ <- emptyByronWalletWith @'Random ctx ("byron3", m3, "Secure Passphrase")

        _ <- emptyWalletWith ctx ("shelley1", "Secure Passphrase", 20)
        _ <- emptyWalletWith ctx ("shelley2", "Secure Passphrase", 20)
        _ <- emptyWalletWith ctx ("shelley3", "Secure Passphrase", 20)

        --list only byron
        rl <- request @[ApiByronWallet] ctx (Link.listWallets @'Byron) Default Empty
        verify rl
            [ expectResponseCode @IO HTTP.status200
            , expectListSize 3
            , expectListField 0
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron1")
            , expectListField 1
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron2")
            , expectListField 2
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron3")
            ]
        --list only shelley
        rl2 <- request @[ApiWallet] ctx (Link.listWallets @'Shelley) Default Empty
        verify rl2
            [ expectResponseCode @IO HTTP.status200
            , expectListSize 3
            , expectListField 0
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley1")
            , expectListField 1
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley2")
            , expectListField 2
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley3")
            ]

    it "BYRON_LIST_04, DELETE_01 -\
        \ Deleted wallets cannot be listed" $ \ctx -> do
        m1 <- genMnemonics @12
        m2 <- genMnemonics @12
        m3 <- genMnemonics @12
        _   <- emptyByronWalletWith @'Random ctx ("byron1", m1, "Secure Passphrase")
        wb2 <- emptyByronWalletWith @'Random ctx ("byron2", m2, "Secure Passphrase")
        _   <- emptyByronWalletWith @'Random ctx ("byron3", m3, "Secure Passphrase")

        _ <- emptyWalletWith ctx ("shelley1", "Secure Passphrase", 20)
        _ <- emptyWalletWith ctx ("shelley2", "Secure Passphrase", 20)
        ws3 <- emptyWalletWith ctx ("shelley3", "Secure Passphrase", 20)

        -- delete
        _ <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron wb2) Default Empty
        _ <- request @ApiWallet ctx (Link.deleteWallet @'Shelley ws3) Default Empty

        --list only byron
        rdl <- request @[ApiByronWallet] ctx (Link.listWallets @'Byron) Default Empty
        verify rdl
            [ expectResponseCode @IO HTTP.status200
            , expectListSize 2
            , expectListField 0
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron1")
            , expectListField 1
                    (#name . #getApiT . #getWalletName) (`shouldBe` "byron3")
            ]
        --list only shelley
        rdl2 <- request @[ApiWallet] ctx (Link.listWallets @'Shelley) Default Empty
        verify rdl2
            [ expectResponseCode @IO HTTP.status200
            , expectListSize 2
            , expectListField 0
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley1")
            , expectListField 1
                    (#name . #getApiT . #getWalletName) (`shouldBe` "shelley2")
            ]

    describe "BYRON_LIST_05 - HTTP headers" $ do
        forM_ (getHeaderCases HTTP.status200)
            $ \(title, headers, expec) -> it title $ \ctx -> do
            w <- emptyRandomWallet ctx
            rl <- request @ApiWallet ctx (Link.getWallet @'Byron w) headers Empty
            verify rl expec

    it "BYRON_DELETE_02 - Byron ep does not delete Shelley wallet" $ \ctx -> do
        w <- emptyWallet ctx
        r <- request @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    it "BYRON_DELETE_03 - Shelley ep does not delete Byron wallet" $ \ctx -> do
        w <- emptyRandomWallet ctx
        r <- request @ApiByronWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
        expectResponseCode @IO HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "BYRON_DELETE_04 - non-existing wallets" $  do
        forM_ falseWalletIds $ \(desc, walId) -> it desc $ \ctx -> do
            w <- emptyRandomWallet ctx
            let endpoint = withPathParam 0 (const $ T.pack walId) $
                    Link.deleteWallet @'Byron w
            rg <- request @ApiByronWallet ctx endpoint Default Empty
            expectResponseCode @IO HTTP.status404 rg
            if (desc == valid40CharHexDesc) then do
                expectErrorMessage (errMsg404NoWallet $ T.pack walId) rg
            else do
                expectErrorMessage errMsg404NoEndpoint rg

    describe "BYRON_DELETE_05 - HTTP headers" $ do
        forM_ (getHeaderCases HTTP.status204)
            $ \(title, headers, expectations) -> it title $ \ctx -> do
            w <- emptyRandomWallet ctx
            rl <- request
                @ApiByronWallet ctx (Link.deleteWallet @'Byron w) headers Empty
            verify rl expectations

    describe "BYRON_RESTORE_01, GET_01, LIST_01 - Restore a wallet" $ do
        let scenarioSuccess endpoint mnemonic ctx = do
                let name = "Empty Byron Wallet"
                let payload = Json [json| {
                        "name": #{name},
                        "mnemonic_sentence": #{mnemonic},
                        "passphrase": "Secure Passphrase"
                    }|]
                let expectations =
                            [ expectField
                                    (#name . #getApiT . #getWalletName) (`shouldBe` name)
                            , expectField (#balance . #available) (`shouldBe` Quantity 0)
                            , expectField (#balance . #total) (`shouldBe` Quantity 0)
                            , expectField #passphrase (`shouldNotBe` Nothing)
                            ]
                -- create
                r <- request @ApiByronWallet ctx endpoint Default payload
                verify r expectations
                let w = getFromResponse id r

                eventually_ $ do
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

        let scenarioFailure endpoint mnemonic ctx = do
                let payload = Json [json| {
                        "name": "Empty Byron Wallet",
                        "mnemonic_sentence": #{mnemonic},
                        "passphrase": "Secure Passphrase"
                    }|]
                r <- request @ApiByronWallet ctx endpoint Default payload
                verify r
                    [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage errMsg400NumberOfWords
                    ]

        let it' endpoint genMnemonicIO test = do
                mnemonic <- runIO genMnemonicIO
                flip it (test endpoint mnemonic) $ unwords
                    [ show endpoint
                    , show (length mnemonic)
                    , "words"
                    ]

        it' (Link.postWallet @'Random) (genMnemonics @9)  scenarioFailure -- ❌
        it' (Link.postWallet @'Random) (genMnemonics @12) scenarioSuccess -- ✔️
        it' (Link.postWallet @'Random) (genMnemonics @15) scenarioFailure -- ❌
        it' (Link.postWallet @'Random) (genMnemonics @18) scenarioFailure -- ❌
        it' (Link.postWallet @'Random) (genMnemonics @21) scenarioFailure -- ❌
        it' (Link.postWallet @'Random) (genMnemonics @24) scenarioFailure -- ❌

        it' (Link.postWallet @'Icarus) (genMnemonics @9)  scenarioFailure -- ❌
        it' (Link.postWallet @'Icarus) (genMnemonics @12) scenarioFailure -- ❌
        it' (Link.postWallet @'Icarus) (genMnemonics @15) scenarioSuccess -- ✔️
        it' (Link.postWallet @'Icarus) (genMnemonics @18) scenarioFailure -- ❌
        it' (Link.postWallet @'Icarus) (genMnemonics @21) scenarioFailure -- ❌
        it' (Link.postWallet @'Icarus) (genMnemonics @24) scenarioFailure -- ❌

        it' (Link.postWallet @'Trezor) (genMnemonics @9)  scenarioFailure -- ❌
        it' (Link.postWallet @'Trezor) (genMnemonics @12) scenarioSuccess -- ✔️
        it' (Link.postWallet @'Trezor) (genMnemonics @15) scenarioSuccess -- ✔️
        it' (Link.postWallet @'Trezor) (genMnemonics @18) scenarioSuccess -- ✔️
        it' (Link.postWallet @'Trezor) (genMnemonics @21) scenarioSuccess -- ✔️
        it' (Link.postWallet @'Trezor) (genMnemonics @24) scenarioSuccess -- ✔️

        it' (Link.postWallet @'Ledger) (genMnemonics @9)  scenarioFailure -- ❌
        it' (Link.postWallet @'Ledger) (genMnemonics @12) scenarioSuccess -- ✔️
        it' (Link.postWallet @'Ledger) (genMnemonics @15) scenarioSuccess -- ✔️
        it' (Link.postWallet @'Ledger) (genMnemonics @18) scenarioSuccess -- ✔️
        it' (Link.postWallet @'Ledger) (genMnemonics @21) scenarioSuccess -- ✔️
        it' (Link.postWallet @'Ledger) (genMnemonics @24) scenarioSuccess -- ✔️

    it "BYRON_RESTORE_02 - One can restore previously deleted wallet" $
        \ctx -> do
            m <- genMnemonics @12
            w <- emptyByronWalletWith @'Random ctx
                ("Byron Wallet", m, "Secure Passphrase")
            rd <- request
                @ApiByronWallet ctx (Link.deleteWallet @'Byron w) Default Empty
            expectResponseCode @IO HTTP.status204 rd
            wr <- emptyByronWalletWith @'Random ctx
                ("Byron Wallet2", m, "Secure Pa33phrase")
            w ^. walletId `shouldBe` wr ^. walletId

    it "BYRON_RESTORE_03 - Cannot restore wallet that exists" $ \ctx -> do
        mnemonic <- genMnemonics @12
        let payload = Json [json| {
                "name": "Some Byron Wallet",
                "mnemonic_sentence": #{mnemonic},
                "passphrase": "Secure Passphrase"
                } |]
        r1 <- request @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
        expectResponseCode @IO HTTP.status201 r1

        r2 <- request @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
        verify r2
            [ expectResponseCode @IO HTTP.status409
            , expectErrorMessage ("This operation would yield a wallet with the\
                 \ following id: " ++ T.unpack (getFromResponse walletId r1) ++
                 " However, I already know of a wallet with this id.")
            ]

    describe "BYRON_RESTORE_04 - Wallet name" $ do
        let walNameMax = T.pack (replicate walletNameMaxLength 'ą')
        let matrix =
                [ ( show walletNameMinLength ++ " char long", "1"
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName) (`shouldBe` "1")
                    ]
                  )
                , ( show walletNameMaxLength ++ " char long", walNameMax
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName) (`shouldBe` walNameMax)
                    ]
                  )
                , ( show (walletNameMaxLength + 1) ++ " char long"
                  , T.pack (replicate (walletNameMaxLength + 1) 'ę')
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "name is too long: expected at\
                            \ most 255 characters"
                    ]
                  )
                , ( "Empty name", ""
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "name is too short: expected at\
                            \ least 1 character"
                     ]
                  )
                , ( "Russian name", russianWalletName
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` russianWalletName)
                    ]
                  )
                , ( "Polish name", polishWalletName
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` polishWalletName)
                    ]
                  )
                , ( "Kanji name", kanjiWalletName
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` kanjiWalletName)
                    ]
                  )
                , ( "Arabic name", arabicWalletName
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` arabicWalletName)
                    ]
                  )
                , ( "Wildcards name", wildcardsWalletName
                  , [ expectResponseCode @IO HTTP.status201
                    , expectField
                            (#name . #getApiT . #getWalletName)
                            (`shouldBe` wildcardsWalletName)
                    ]
                  )
                ]
        forM_ matrix $ \(title, walName, expectations) -> it title $ \ctx -> do
            let payload = Json [json| {
                    "name": #{walName},
                    "mnemonic_sentence": #{mnemonics12},
                    "passphrase": "Secure Passphrase"
                    } |]
            r <- request @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
            verify r expectations

    it "BYRON_RESTORE_04 - [] as name -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": [],
                "mnemonic_sentence": #{mnemonics12},
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Text, encountered Array"
            ]

    it "BYRON_RESTORE_04 - Num as name -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": 123,
                "mnemonic_sentence": #{mnemonics12},
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Text, encountered Number"
            ]

    it "BYRON_RESTORE_04 - Name param missing -> fail" $ \ctx -> do
        let payload = Json [json| {
                "mnemonic_sentence": #{mnemonics12},
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "key 'name' not present"
            ]

    describe "BYRON_RESTORE_05 - Faulty mnemonics" $ do
        let matrix =
             [ ( "[] as mnemonic_sentence -> fail", []
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage
                    "Invalid number of words: 12 words are expected"
                 ]
               )
             , ( "specMnemonicSentence -> fail", specMnemonicByron
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Invalid entropy checksum: please \
                      \double-check the last word of your mnemonic sentence."
                 ]
               )
             , ( "invalid mnemonics -> fail", invalidMnemonics12
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage "Invalid entropy checksum: please \
                      \double-check the last word of your mnemonic sentence."
                 ]
               )
             , ( "Japanese mnemonics -> fail", japaneseMnemonics12
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage errMsgNotInDictionary
                 ]
               )
             , ( "French mnemonics -> fail"
               , frenchMnemonics12
               , [ expectResponseCode @IO HTTP.status400
                 , expectErrorMessage errMsgNotInDictionary
                 ]
               )
             ]

        forM_ matrix $ \(title, mnemonics, expectations) -> it title $
            \ctx -> do
                let payload = Json [json| {
                        "name": "Byron Wallet bye bye",
                        "mnemonic_sentence": #{mnemonics},
                        "passphrase": "Secure Passphrase"
                        } |]
                r <- request
                    @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
                verify r expectations

    it "BYRON_RESTORE_05 - String as mnemonic_sentence -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "ads",
                "mnemonic_sentence": "album execute kingdom dumb trip",
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected [a], encountered String"
            ]

    it "BYRON_RESTORE_05 - Num as mnemonic_sentence -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "123",
                "mnemonic_sentence": 15,
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected [a], encountered Number"
            ]

    it "BYRON_RESTORE_05 - mnemonic_sentence param missing -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "A name",
                "passphrase": "Secure Passphrase"
                } |]
        r <- request @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "key 'mnemonic_sentence' not present"
            ]

    describe "BYRON_RESTORE_06 - Passphrase" $ do
        let minLength = passphraseMinLength (Proxy @"encryption")
        let maxLength = passphraseMaxLength (Proxy @"encryption")
        let matrix =
                [ ( show minLength ++ " char long"
                  , T.pack (replicate minLength 'ź')
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                , ( show (minLength - 1) ++ " char long"
                  , T.pack (replicate (minLength - 1) 'ż')
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "passphrase is too short: expected at\
                            \ least 10 characters"
                    ]
                  )
                , ( show maxLength ++ " char long"
                  , T.pack (replicate maxLength 'ą')
                  , [ expectResponseCode @IO HTTP.status201 ]
                  )
                , ( show (maxLength + 1) ++ " char long"
                  , T.pack (replicate (maxLength + 1) 'ę')
                  , [ expectResponseCode @IO HTTP.status400
                    , expectErrorMessage "passphrase is too long: expected at\
                            \ most 255 characters"
                    ]
                  )
                , ( "Empty passphrase", ""
                   , [ expectResponseCode @IO HTTP.status400
                     , expectErrorMessage "passphrase is too short: expected at\
                            \ least 10 characters"
                     ]
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
                        "passphrase": #{passphrase}
                        } |]
                r <- request
                    @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
                verify r expectations

    it "BYRON_RESTORE_06 - [] as passphrase -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics12},
                "passphrase": []
                } |]
        r <- request @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Text, encountered Array"
            ]

    it "BYRON_RESTORE_06 - Num as passphrase -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics12},
                "passphrase": 777
                } |]
        r <- request @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "expected Text, encountered Number"
            ]

    it "BYRON_RESTORE_06 - passphrase param missing -> fail" $ \ctx -> do
        let payload = Json [json| {
                "name": "Secure Wallet",
                "mnemonic_sentence": #{mnemonics12}
                } |]
        r <- request @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status400
            , expectErrorMessage "key 'passphrase' not present"
            ]

    describe "BYRON_RESTORE_07 - HTTP headers" $ do
        forM_ postHeaderCases $ \(title, headers, expectations) -> it title $ \ctx -> do
            let payload = Json [json| {
                    "name": "Secure Wallet",
                    "mnemonic_sentence": #{mnemonics12},
                    "passphrase": "Secure passphrase"
                    } |]
            r <- request @ApiByronWallet ctx (Link.postWallet @'Random) headers payload
            verify r expectations

    describe "BYRON_RESTORE_07 - Bad request" $ do
        let matrix =
                [ ( "empty payload", NonJson "" )
                , ( "{} payload", NonJson "{}" )
                , ( "non-json valid payload"
                  , NonJson
                        "{name: wallet,\
                        \ mnemonic_sentence: [pill, hand, ask, useless, asset,\
                        \ rely, above, pipe, embark, game, elder, unaware,\
                        \ nasty, coach, glad],\
                        \ passphrase: 1234567890}"
                  )
                ]

        forM_ matrix $ \(name, nonJson) -> it name $ \ctx -> do
            let payload = nonJson
            r <- request @ApiByronWallet ctx (Link.postWallet @'Random) Default payload
            expectResponseCode @IO HTTP.status400 r

    describe "BYRON_RESTORE_07, LIST_05 - listWallets Methods Not Allowed" $ do
        let matrix = ["PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            let endpoint = withMethod method $ Link.listWallets @'Byron
            r <- request @ApiByronWallet ctx endpoint Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

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
                "passphrase": #{fixturePassphrase}
                } |]

        r <- request @ApiByronWallet ctx (Link.postWallet @'Icarus) Default payload
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
                "passphrase": #{fixturePassphrase}
                } |]

        r <- request @ApiByronWallet ctx (Link.postWallet @'Ledger) Default payload
        verify r
            [ expectResponseCode @IO HTTP.status201
            , expectField (#balance . #available) (`shouldBe` Quantity faucetAmt)
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

valid40CharHexDesc :: String
valid40CharHexDesc = "40 chars hex"
