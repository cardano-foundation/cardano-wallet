{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Byron.Migrations
    ( spec
    ) where

import Prelude

import Cardano.Mnemonic
    ( entropyToMnemonic, genEntropy )
import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiTransaction
    , ApiUtxoStatistics
    , ApiWallet
    , ApiWalletMigrationInfo (..)
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Maybe
    ( mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import Test.Hspec
    ( SpecWith, describe, shouldBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , emptyIcarusWallet
    , emptyRandomWallet
    , emptyWallet
    , eventually
    , expectErrorMessage
    , expectField
    , expectResponseCode
    , fixtureIcarusWallet
    , fixturePassphrase
    , fixtureRandomWallet
    , getFromResponse
    , icarusAddresses
    , json
    , listAddresses
    , postByronWallet
    , randomAddresses
    , request
    , unsafeResponse
    , verify
    , walletId
    , (.>)
    )
import Test.Integration.Framework.TestData
    ( errMsg400ParseError
    , errMsg403NothingToMigrate
    , errMsg403WrongPass
    , errMsg404NoWallet
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Api.Types as ApiTypes
import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Types.Status as HTTP
import qualified Test.Hspec as Hspec


spec :: forall n.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n ShelleyKey
    , PaymentAddress n IcarusKey
    , PaymentAddress n ByronKey
    ) => SpecWith Context
spec = describe "BYRON_MIGRATIONS" $ do
    it "BYRON_CALCULATE_01 - \
        \for non-empty wallet calculated fee is > zero."
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> runResourceT $ do
            w <- fixtureByronWallet ctx
            let ep = Link.getMigrationInfo @'Byron w
            r <- request @ApiWalletMigrationInfo ctx ep Default Empty
            verify r
                [ expectResponseCode HTTP.status200
                , expectField (#migrationCost . #getQuantity)
                    (.> 0)
                ]

    it "BYRON_CALCULATE_02 - \
        \Cannot calculate fee for empty wallet."
        $ \ctx -> forM_ [emptyRandomWallet, emptyIcarusWallet]
        $ \emptyByronWallet -> runResourceT $ do
            w <- emptyByronWallet ctx
            let ep = Link.getMigrationInfo @'Byron w
            r <- request @ApiWalletMigrationInfo ctx ep Default Empty
            verify r
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate $ w ^. walletId)
                ]

    it "BYRON_CALCULATE_02 - \
        \Cannot calculate fee for wallet with dust, that cannot be migrated."
        $ \ctx -> runResourceT $ do
            -- NOTE
            -- Special mnemonic for which wallet with dust
            -- (5 utxo with 60 lovelace)
            let mnemonics =
                    ["suffer", "decorate", "head", "opera", "yellow", "debate"
                    , "visa", "fire", "salute", "hybrid", "stone", "smart"] :: [Text]
            let payloadRestore = Json [json| {
                    "name": "Dust Byron Wallet",
                    "mnemonic_sentence": #{mnemonics},
                    "passphrase": #{fixturePassphrase},
                    "style": "random"
                    } |]
            w <- unsafeResponse <$> postByronWallet ctx payloadRestore
            let ep = Link.getMigrationInfo @'Byron w
            r <- request @ApiWalletMigrationInfo ctx ep Default Empty
            verify r
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate $ w ^. walletId)
                ]

    it "BYRON_CALCULATE_03 - \
        \Cannot estimate migration for Shelley wallet using Byron endpoint"
        $ \ctx -> runResourceT $ do
            w <- emptyWallet ctx
            let ep = Link.getMigrationInfo @'Byron w
            r <- request @ApiWalletMigrationInfo ctx ep Default Empty
            expectResponseCode HTTP.status404 r
            expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "BYRON_MIGRATE_05 - I could migrate to any valid address" $ do
        forM_ [ ("Byron", emptyRandomWallet)
              , ("Icarus", emptyIcarusWallet)
              ] $ \(walType, destWallet) -> do

            it ("From wallet type: " ++ walType) $ \ctx -> runResourceT $ do
                --shelley address
                wShelley <- emptyWallet ctx
                addrs <- listAddresses @n ctx wShelley
                let addrShelley = (addrs !! 1) ^. #id
                --icarus address
                addrIcarus <- liftIO $ encodeAddress @n . head . icarusAddresses @n
                    . entropyToMnemonic @15 <$> genEntropy
                --byron address
                addrByron <- liftIO $ encodeAddress @n . head . randomAddresses @n
                    . entropyToMnemonic @12 <$> genEntropy

                sWallet <- destWallet ctx
                r <- request @[ApiTransaction n] ctx
                    (Link.migrateWallet @'Byron sWallet)
                    Default
                    (Json [json|
                        { passphrase: #{fixturePassphrase}
                        , addresses: [#{addrShelley}, #{addrIcarus}, #{addrByron}]
                        }|])
                verify r
                    [ expectResponseCode HTTP.status403
                    , expectErrorMessage
                        (errMsg403NothingToMigrate (sWallet ^. walletId))
                    ]

    it "BYRON_MIGRATE_07 - invalid payload, parser error" $ \ctx -> runResourceT $ do
        sourceWallet <- emptyRandomWallet ctx

        r <- request @[ApiTransaction n] ctx
            (Link.migrateWallet @'Byron sourceWallet)
            Default
            (NonJson "{passphrase:,}")
        expectResponseCode HTTP.status400 r
        expectErrorMessage errMsg400ParseError r

    it "BYRON_MIGRATE_01 - \
        \after a migration operation successfully completes, the correct \
        \amount eventually becomes available in the target wallet for arbitrary \
        \ number of specified addresses."
        $ \ctx -> runResourceT $ do
              testAddressCycling ctx 1
              testAddressCycling ctx 3
              testAddressCycling ctx 10

    Hspec.it "BYRON_MIGRATE_01 - \
        \ migrate a big wallet requiring more than one tx" $ \ctx -> runResourceT @IO $ do
        -- NOTE
        -- Special mnemonic for which 200 legacy funds are attached to in the
        -- genesis file.
        --
        -- Out of these 200 coins, 100 of them are of 1 Lovelace and are
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
                "passphrase": #{fixturePassphrase},
                "style": "random"
                } |]
        wOld <- unsafeResponse <$> postByronWallet ctx payloadRestore
        originalBalance <- eventually "wallet balance greater than 0" $ do
            r <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron wOld)
                Default
                Empty
            verify r
                [ expectField (#balance . #available) (.> Quantity 0)
                ]
            return $ getFromResponse (#balance . #available . #getQuantity) r

        --Calculate the expected migration fee:
        rFee <- request @ApiWalletMigrationInfo ctx
            (Link.getMigrationInfo @'Byron wOld)
            Default
            Empty
        verify rFee
            [ expectResponseCode HTTP.status200
            , expectField #migrationCost (.> Quantity 0)
            ]
        let expectedFee = getFromResponse (#migrationCost . #getQuantity) rFee
        let leftovers = getFromResponse (#leftovers . #getQuantity) rFee

        -- Migrate to a new empty wallet
        wNew <- emptyWallet ctx
        addrs <- listAddresses @n ctx wNew
        let addr1 = (addrs !! 1) ^. #id

        let payloadMigrate =
                Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: [#{addr1}]
                    }|]
        rm <- request @[ApiTransaction n] ctx
            (Link.migrateWallet @'Byron wOld)
            Default
            payloadMigrate
        verify rm
            [ expectResponseCode HTTP.status202
            , expectField id ( (`shouldBe` 19) . length )
            ]

        -- Check that funds become available in the target wallet:
        let expectedBalance = originalBalance - expectedFee - leftovers
        eventually "wallet balance = expectedBalance" $ do
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
        request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Shelley wNew)
            Default
            Empty >>= flip verify
            [ expectField
                #distribution
                ((`shouldBe` (Just 100)) . Map.lookup 10_000_000_000)
            ]

    it "BYRON_MIGRATE_01 - \
        \a migration operation removes all funds from the source wallet."
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> runResourceT $ do
            -- Restore a Byron wallet with funds, to act as a source wallet:
            sourceWallet <- fixtureByronWallet ctx

            -- Perform a migration from the source wallet to a target wallet:
            targetWallet <- emptyWallet ctx
            addrs <- listAddresses @n ctx targetWallet
            let addr1 = (addrs !! 1) ^. #id

            r0 <- request @[ApiTransaction n] ctx
                (Link.migrateWallet @'Byron sourceWallet)
                Default
                (Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: [#{addr1}]
                    }|])
            verify r0
                [ expectResponseCode HTTP.status202
                , expectField id (`shouldSatisfy` (not . null))
                ]

            -- Verify that the source wallet has no funds available:
            r1 <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron sourceWallet) Default Empty
            verify r1
                [ expectResponseCode HTTP.status200
                , expectField (#balance . #available) (`shouldBe` Quantity 0)
                ]

    it "BYRON_MIGRATE_02 - \
        \migrating an empty wallet should fail."
        $ \ctx -> forM_ [emptyRandomWallet, emptyIcarusWallet]
        $ \emptyByronWallet -> runResourceT $ do
            sourceWallet <- emptyByronWallet ctx
            targetWallet <- emptyWallet ctx
            addrs <- listAddresses @n ctx targetWallet
            let addr1 = (addrs !! 1) ^. #id
            let payload =
                    Json [json|
                        { passphrase: #{fixturePassphrase}
                        , addresses: [#{addr1}]
                        }|]
            let ep = Link.migrateWallet @'Byron sourceWallet
            r <- request @[ApiTransaction n] ctx ep Default payload
            let srcId = sourceWallet ^. walletId
            verify r
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate srcId)
                ]

    Hspec.it "BYRON_MIGRATE_02 - \
        \migrating wallet with dust should fail."
        $ \ctx -> runResourceT @IO $ do
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
                    "passphrase": #{fixturePassphrase},
                    "style": "random"
                    } |]
            sourceWallet <- unsafeResponse <$> postByronWallet ctx payloadRestore
            eventually "wallet balance greater than 0" $ do
                request @ApiByronWallet ctx
                    (Link.getWallet @'Byron sourceWallet)
                    Default
                    Empty >>= flip verify
                    [ expectField (#balance . #available) (.> Quantity 0)
                    ]

            targetWallet <- emptyWallet ctx
            addrs <- listAddresses @n ctx targetWallet
            let addr1 = (addrs !! 1) ^. #id
            let payload =
                    Json [json|
                        { passphrase: #{fixturePassphrase}
                        , addresses: [#{addr1}]
                        }|]
            let ep = Link.migrateWallet @'Byron sourceWallet
            r <- request @[ApiTransaction n] ctx ep Default payload
            let srcId = sourceWallet ^. walletId
            verify r
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate srcId)
                ]

    it "BYRON_MIGRATE_03 - \
        \actual fee for migration is the same as the predicted fee."
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> runResourceT $ do
            -- Restore a Byron wallet with funds.
            sourceWallet <- fixtureByronWallet ctx

            -- Request a migration fee prediction.
            let ep0 = (Link.getMigrationInfo @'Byron sourceWallet)
            r0 <- request @ApiWalletMigrationInfo ctx ep0 Default Empty
            verify r0
                [ expectResponseCode HTTP.status200
                , expectField #migrationCost (.> Quantity 0)
                ]

            -- Perform the migration.
            targetWallet <- emptyWallet ctx
            addrs <- listAddresses @n ctx targetWallet
            let addr1 = (addrs !! 1) ^. #id
            let payload =
                    Json [json|
                        { passphrase: #{fixturePassphrase}
                        , addresses: [#{addr1}]
                        }|]
            let ep1 = Link.migrateWallet @'Byron sourceWallet
            r1 <- request @[ApiTransaction n] ctx ep1 Default payload
            verify r1
                [ expectResponseCode HTTP.status202
                , expectField id (`shouldSatisfy` (not . null))
                ]

            -- Verify that the fee prediction was correct.
            let actualFee = fromIntegral $ sum $ apiTransactionFee
                    <$> getFromResponse id r1
            let predictedFee =
                    getFromResponse (#migrationCost . #getQuantity) r0
            liftIO $ actualFee `shouldBe` predictedFee

    it "BYRON_MIGRATE_04 - migration fails with a wrong passphrase"
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> runResourceT $ do
        -- Restore a Byron wallet with funds, to act as a source wallet:
        sourceWallet <- fixtureByronWallet ctx

        -- Perform a migration from the source wallet to a target wallet:
        targetWallet <- emptyWallet ctx
        addrs <- listAddresses @n ctx targetWallet
        let addr1 = (addrs !! 1) ^. #id
        r0 <- request @[ApiTransaction n] ctx
            (Link.migrateWallet @'Byron sourceWallet)
            Default
            (Json [json|
                { passphrase: "not-the-right-passphrase"
                , addresses: [#{addr1}]
                }|])
        verify r0
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

  where
    -- Compute the fee associated with an API transaction.
    apiTransactionFee :: ApiTransaction n -> Word64
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

    testAddressCycling ctx addrNum =
        forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> runResourceT $ do
            -- Restore a Byron wallet with funds, to act as a source wallet:
            sourceWallet <- fixtureByronWallet ctx
            let originalBalance =
                        view (#balance . #available . #getQuantity) sourceWallet

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            addrs <- listAddresses @n ctx targetWallet
            let addrIds =
                    map (\(ApiTypes.ApiAddress theid _) -> theid) $
                    take addrNum addrs

            -- Calculate the expected migration fee:
            r0 <- request @ApiWalletMigrationInfo ctx
                (Link.getMigrationInfo @'Byron sourceWallet) Default Empty
            verify r0
                [ expectResponseCode HTTP.status200
                , expectField #migrationCost (.> Quantity 0)
                ]
            let expectedFee = getFromResponse (#migrationCost . #getQuantity) r0
            let leftovers = getFromResponse (#leftovers . #getQuantity) r0

            -- Perform a migration from the source wallet to the target wallet:
            r1 <- request @[ApiTransaction n] ctx
                (Link.migrateWallet @'Byron sourceWallet)
                Default
                (Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: #{addrIds}
                    }|])
            verify r1
                [ expectResponseCode HTTP.status202
                , expectField id (`shouldSatisfy` (not . null))
                ]

            -- Check that funds become available in the target wallet:
            let expectedBalance = originalBalance - expectedFee - leftovers
            eventually "Wallet has expectedBalance" $ do
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
