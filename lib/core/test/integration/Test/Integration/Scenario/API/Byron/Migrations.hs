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
    , ApiEra (..)
    , ApiT (..)
    , ApiTransaction
    , ApiTxInput (source)
    , ApiUtxoStatistics
    , ApiWallet
    , ApiWalletMigrationPlan (..)
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
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxStatus (..) )
import Control.Monad
    ( forM_, void, when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Functor
    ( (<&>) )
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
    , unsafeRequest
    , unsafeResponse
    , verify
    , waitForTxImmutability
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

    it "BYRON_CREATE_MIGRATION_PLAN_01 - \
        \Can create a migration plan."
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> runResourceT $ do
            sourceWallet <- fixtureByronWallet ctx
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Byron sourceWallet
            response <- request @(ApiWalletMigrationPlan n) ctx ep Default
                (Json [json|{addresses: #{targetAddressIds}}|])
            verify response
                [ expectResponseCode HTTP.status202
                , expectField (#totalFee . #getQuantity)
                    (`shouldBe`
                        if _mainEra ctx >= ApiBabbage
                        then 334_100
                        else 333_900)
                , expectField (#selections)
                    ((`shouldBe` 1) . length)
                , expectField (#balanceSelected . #ada . #getQuantity)
                    (`shouldBe` 1_000_000_000_000)
                , expectField (#balanceLeftover . #ada . #getQuantity)
                    (`shouldBe` 0)
                ]

    it "BYRON_CREATE_MIGRATION_PLAN_02 - \
        \Cannot create plan for empty wallet."
        $ \ctx -> forM_ [emptyRandomWallet, emptyIcarusWallet]
        $ \emptyByronWallet -> runResourceT $ do
            sourceWallet <- emptyByronWallet ctx
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Byron sourceWallet
            response <- request @(ApiWalletMigrationPlan n) ctx ep Default
                (Json [json|{addresses: #{targetAddressIds}}|])
            verify response
                [ expectResponseCode HTTP.status403
                , expectErrorMessage
                    (errMsg403NothingToMigrate $ sourceWallet ^. walletId)
                ]

    it "BYRON_CREATE_MIGRATION_PLAN_03 - \
        \Cannot create plan for Shelley wallet using Byron endpoint."
        $ \ctx -> runResourceT $ do
            sourceWallet <- emptyWallet ctx
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Byron sourceWallet
            response <- request @(ApiWalletMigrationPlan n) ctx ep Default
                (Json [json|{addresses: #{targetAddressIds}}|])
            verify response
                [ expectResponseCode HTTP.status404
                , expectErrorMessage
                    (errMsg404NoWallet $ sourceWallet ^. walletId)
                ]

    it "BYRON_CREATE_MIGRATION_PLAN_04 - \
        \Cannot create a plan for a wallet that only contains dust."
        $ \ctx -> runResourceT $ do
            -- NOTE:
            -- Special mnemonic for wallet that has dust
            -- (5 UTxOs with 60 lovelace each)
            let mnemonicSentence =
                    [ "suffer", "decorate", "head", "opera", "yellow", "debate"
                    , "visa", "fire", "salute", "hybrid", "stone", "smart"
                    ] :: [Text]
            let payloadRestore = Json [json| {
                    "name": "Dust Byron Wallet",
                    "mnemonic_sentence": #{mnemonicSentence},
                    "passphrase": #{fixturePassphrase},
                    "style": "random"
                    } |]
            sourceWallet <- unsafeResponse <$>
                postByronWallet ctx payloadRestore
            eventually "sourceWallet contains dust" $
                request @ApiByronWallet ctx
                    (Link.getWallet @'Byron sourceWallet) Default Empty
                    >>= flip verify
                    [ expectField (#balance . #available . #getQuantity)
                        (.> 0)
                    ]

            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Byron sourceWallet
            response <- request @(ApiWalletMigrationPlan n) ctx ep Default
                (Json [json|{addresses: #{targetAddressIds}}|])
            verify response
                [ expectResponseCode HTTP.status403
                , expectErrorMessage
                    (errMsg403NothingToMigrate $ sourceWallet ^. walletId)
                ]

    it "BYRON_CREATE_MIGRATION_PLAN_05 - \
        \Creating a plan is deterministic."
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> runResourceT $ do
            sourceWallet <- fixtureByronWallet ctx
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Byron sourceWallet
            response1 <- request @(ApiWalletMigrationPlan n) ctx ep Default
                (Json [json|{addresses: #{targetAddressIds}}|])
            response2 <- request @(ApiWalletMigrationPlan n) ctx ep Default
                (Json [json|{addresses: #{targetAddressIds}}|])
            expectResponseCode HTTP.status202 response1
            expectResponseCode HTTP.status202 response2
            expectField (#selections) ((.> 0) . length) response1
            expectField (#selections) ((.> 0) . length) response2
            case (snd response1, snd response2) of
                (Right plan1, Right plan2) ->
                    liftIO $ plan1 `shouldBe` plan2
                _ ->
                    error "Unable to compare plans."

    describe "BYRON_MIGRATE_01 - \
        \After a migration operation successfully completes, the correct \
        \amounts eventually become available in the target wallet for an \
        \arbitrary number of specified addresses, and the balance of the \
        \source wallet is completely depleted."
        $ do
            testAddressCycling "Random" fixtureRandomWallet  1
            testAddressCycling "Random" fixtureRandomWallet  3
            testAddressCycling "Random" fixtureRandomWallet 10
            testAddressCycling "Icarus" fixtureIcarusWallet  1
            testAddressCycling "Icarus" fixtureIcarusWallet  3
            testAddressCycling "Icarus" fixtureIcarusWallet 10

    Hspec.it "BYRON_MIGRATE_02 - \
        \Can migrate a large wallet requiring more than one transaction."
        $ \ctx -> runResourceT @IO $ do

        -- NOTE:
        --
        -- Special mnemonic to which 200 legacy coins are attached in the
        -- genesis file.
        --
        -- Out of these 200 coins:
        --
        --  - 100 coins are each worth 1 lovelace, and are expected to be
        --    treated as dust.
        --  - 100 coins are each worth 10,000,000,000 lovelace.
        --
        let sourceWalletMnemonic =
                ["collect", "fold", "file", "clown"
                , "injury", "sun", "brass", "diet"
                , "exist", "spike", "behave", "clip"
                ] :: [Text]
        sourceWallet <- unsafeResponse <$> postByronWallet ctx
            (Json [json|{
                "name": "Big Byron Wallet",
                "mnemonic_sentence": #{sourceWalletMnemonic},
                "passphrase": #{fixturePassphrase},
                "style": "random"
            }|])
        sourceBalance <- eventually "Source wallet balance is correct." $ do
            response <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron sourceWallet) Default Empty
            verify response
                [ expectField (#balance . #available . #getQuantity)
                    (`shouldBe` 1_000_000_000_100)
                ]
            return $ getFromResponse
                (#balance . #available . #getQuantity) response

        -- Create an empty target wallet:
        targetWallet <- emptyWallet ctx
        targetAddresses <- listAddresses @n ctx targetWallet
        let targetAddressIds = targetAddresses <&>
                (\(ApiTypes.ApiAddress addrId _ _) -> addrId)

        -- Compute the expected migration plan:
        responsePlan <- request @(ApiWalletMigrationPlan n) ctx
            (Link.createMigrationPlan @'Byron sourceWallet) Default
            (Json [json|{addresses: #{targetAddressIds}}|])
        verify responsePlan
            [ expectResponseCode HTTP.status202
            , expectField
                (#totalFee . #getQuantity)
                (`shouldBe`
                    if _mainEra ctx >= ApiBabbage
                    then 2_460_200
                    else 2_459_800)
            , expectField
                (#selections)
                ((`shouldBe` 2) . length)
            , expectField
                (#balanceLeftover . #ada . #getQuantity)
                (`shouldBe` 100)
            , expectField
                (#balanceSelected . #ada . #getQuantity)
                (`shouldBe` 1_000_000_000_000)
            ]
        let expectedFee = getFromResponse
                (#totalFee . #getQuantity) responsePlan
        let balanceLeftover =getFromResponse
                (#balanceLeftover . #ada . #getQuantity) responsePlan

        -- Perform a migration from the source wallet to the target wallet.
        --
        -- This migration will involve more than one transaction, where each
        -- transaction is sent one by one. It may happen that one of these
        -- transactions is rolled back or simply discarded entirely. The wallet
        -- doesn't currently have any retry mechanism, which means that
        -- transactions must be manually retried by clients.
        --
        -- The 'migrateWallet' function tries do exactly that: to make sure
        -- that rolled-back transactions are cancelled and retried until the
        -- migration is complete.
        --
        liftIO $ migrateWallet ctx sourceWallet targetAddressIds
        waitForTxImmutability ctx

        -- Check that funds become available in the target wallet.
        let expectedTargetBalance =
                sourceBalance - balanceLeftover - expectedFee
        request @ApiWallet ctx
            (Link.getWallet @'Shelley targetWallet) Default Empty
            >>= flip verify
            [ expectField
                (#balance . #available . #getQuantity)
                (`shouldBe` expectedTargetBalance)
            , expectField
                (#balance . #total . #getQuantity)
                (`shouldBe` expectedTargetBalance)
            ]

        -- Analyse the target wallet's UTxO distribution:
        responseStats <- request @ApiUtxoStatistics ctx
            (Link.getUTxOsStatistics @'Shelley targetWallet) Default Empty
        verify responseStats
            [ expectField
                (#distribution)
                ((`shouldBe` (Just 2)) . Map.lookup 1_000_000_000_000)
            ]

        -- Check that the source wallet has the expected leftover balance:
        responseFinalSourceBalance <- request @ApiByronWallet ctx
            (Link.getWallet @'Byron sourceWallet) Default Empty
        verify responseFinalSourceBalance
            [ expectResponseCode HTTP.status200
            , expectField (#balance . #available)
                (`shouldBe` Quantity 100)
            , expectField (#balance . #total)
                (`shouldBe` Quantity 100)
            ]

    it "BYRON_MIGRATE_03 - \
        \Migrating an empty wallet should fail."
        $ \ctx -> forM_ [emptyRandomWallet, emptyIcarusWallet]
        $ \emptyByronWallet -> runResourceT $ do
            sourceWallet <- emptyByronWallet ctx
            let sourceWalletId = sourceWallet ^. walletId
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)
            let ep = Link.migrateWallet @'Byron sourceWallet
            response <- request @[ApiTransaction n] ctx ep Default $
                Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: #{targetAddressIds}
                    }|]
            verify response
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate sourceWalletId)
                ]

    Hspec.it "BYRON_MIGRATE_04 - \
        \Actual fee for migration is identical to predicted fee."
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> runResourceT @IO $ do

            let feeExpected =
                    if _mainEra ctx >= ApiBabbage
                    then 334_100
                    else 333_900

            -- Restore a source wallet with funds.
            sourceWallet <- fixtureByronWallet ctx

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)

            -- Create a migration plan:
            let endpointPlan = (Link.createMigrationPlan @'Byron sourceWallet)
            responsePlan <- request @(ApiWalletMigrationPlan n)
                ctx endpointPlan Default $
                Json [json|{addresses: #{targetAddressIds}}|]
            verify responsePlan
                [ expectResponseCode HTTP.status202
                , expectField #totalFee (`shouldBe` Quantity feeExpected)
                , expectField #selections ((`shouldBe` 1) . length)
                ]

            -- Perform a migration:
            let endpointMigrate = Link.migrateWallet @'Byron sourceWallet
            responseMigrate <-
                request @[ApiTransaction n] ctx endpointMigrate Default $
                Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: #{targetAddressIds}
                    }|]
            -- Verify the fee is as expected:
            verify responseMigrate
                [ expectResponseCode HTTP.status202
                , expectField id ((`shouldBe` 1) . length)
                , expectField id
                    $ (`shouldBe` feeExpected)
                    . fromIntegral
                    . sum
                    . fmap apiTransactionFee
                ]

    it "BYRON_MIGRATE_05 - \
        \Migration fails if the wrong passphrase is supplied."
        $ \ctx -> forM_ [fixtureRandomWallet, fixtureIcarusWallet]
        $ \fixtureByronWallet -> runResourceT $ do

            -- Restore a Byron wallet with funds, to act as a source wallet:
            sourceWallet <- fixtureByronWallet ctx

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)

            -- Attempt to perform a migration:
            response <- request @[ApiTransaction n] ctx
                (Link.migrateWallet @'Byron sourceWallet)
                Default
                (Json [json|
                    { passphrase: "not-the-right-passphrase"
                    , addresses: #{targetAddressIds}
                    }|])
            verify response
                [ expectResponseCode HTTP.status403
                , expectErrorMessage errMsg403WrongPass
                ]

    describe "BYRON_MIGRATE_06 - \
        \It's possible to migrate to any valid address."
        $ forM_
              [ ("Random", emptyRandomWallet)
              , ("Icarus", emptyIcarusWallet)
              ] $ \(walType, destWallet) ->

            it ("From wallet type: " ++ walType) $ \ctx -> runResourceT $ do

                -- Create a Shelley address:
                wShelley <- emptyWallet ctx
                addrs <- listAddresses @n ctx wShelley
                let addrShelley = (addrs !! 1) ^. #id

                -- Create an Icarus address:
                addrIcarus <- liftIO $ encodeAddress @n
                    . head
                    . icarusAddresses @n
                    . entropyToMnemonic @15 <$> genEntropy

                -- Create a Byron address:
                addrByron <- liftIO $ encodeAddress @n
                    . head
                    . randomAddresses @n
                    . entropyToMnemonic @12 <$> genEntropy

                -- Create a source wallet:
                sourceWallet <- destWallet ctx

                -- Initiate a migration to all address types:
                response <- request @[ApiTransaction n] ctx
                    (Link.migrateWallet @'Byron sourceWallet) Default
                    (Json [json|
                        { passphrase: #{fixturePassphrase}
                        , addresses:
                            [ #{addrShelley}
                            , #{addrIcarus}
                            , #{addrByron}
                            ]
                        }|])
                verify response
                    [ expectResponseCode HTTP.status403
                    , expectErrorMessage
                        (errMsg403NothingToMigrate (sourceWallet ^. walletId))
                    ]

    it "BYRON_MIGRATE_07 - \
        \Including an invalidly-formatted passphrase results in a parser error."
        $ \ctx -> runResourceT $ do
            sourceWallet <- emptyRandomWallet ctx
            response <- request @[ApiTransaction n] ctx
                (Link.migrateWallet @'Byron sourceWallet) Default
                (NonJson "{passphrase:,}")
            verify response
                [ expectResponseCode HTTP.status400
                , expectErrorMessage errMsg400ParseError
                ]

    Hspec.it "BYRON_MIGRATE_08 - \
        \It's not possible to migrate a wallet whose total balance is less \
        \than the minimum ada quantity for an output."
        $ \ctx -> runResourceT @IO $ do

            -- Create a source wallet with a small number of small quantities:
            let mnemonicSentence =
                    [ "suffer", "decorate", "head", "opera"
                    , "yellow", "debate", "visa", "fire"
                    , "salute", "hybrid", "stone", "smart"
                    ] :: [Text]
            sourceWallet <- unsafeResponse <$> postByronWallet ctx
                (Json [json|{
                    "name": "Dust Byron Wallet",
                    "mnemonic_sentence": #{mnemonicSentence},
                    "passphrase": #{fixturePassphrase},
                    "style": "random"
                }|])
            eventually "Source wallet balance is correct." $ do
                request @ApiByronWallet ctx
                    (Link.getWallet @'Byron sourceWallet)
                    Default
                    Empty >>= flip verify
                    [ expectField (#balance . #available)
                        (`shouldBe` Quantity 15)
                    ]
            let sourceWalletId = sourceWallet ^. walletId

            -- Analyse the source wallet's UTxO distribution:
            let expectedSourceDistribution = [(10, 5)]
            responseSourceDistribution <- request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics @'Byron sourceWallet) Default Empty
            verify responseSourceDistribution
                [ expectField #distribution
                    ((`shouldBe` expectedSourceDistribution)
                    . Map.toList
                    . Map.filter (> 0)
                    )
                ]

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)


            -- Attempt a migration:
            let ep = Link.migrateWallet @'Byron sourceWallet
            responseMigrate <- request @[ApiTransaction n] ctx ep Default $
                Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: #{targetAddressIds}
                    }|]
            verify responseMigrate
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate sourceWalletId)
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
            . mapMaybe source
            . view #inputs
        outputBalance = fromIntegral
            . sum
            . fmap (view (#amount . #getQuantity))
            . view #outputs

    migrateWallet
        :: Context
        -> ApiByronWallet
        -> [(ApiT Address, Proxy n)]
        -> IO ()
    migrateWallet ctx src targets = do
        (status, _) <- request @(ApiWalletMigrationPlan n) ctx
            endpointCreateMigrationPlan Default payloadCreateMigrationPlan
        when (status == HTTP.status202) $ do
            -- The above request returns '403 Nothing to Migrate' when done.

            -- 1. Forget all pending transactions to unlock any locked UTxO:
            (_, txs) <- unsafeRequest
                @[ApiTransaction n] ctx endpointListTxs Empty
            forM_ txs $ forgetTxIf ((== ApiT Pending) . view #status)

            -- 2. Attempt to migrate:
            _ <- request @[ApiTransaction n] ctx endpointMigrateWallet Default
                payloadMigrateWallet

            -- 3. Wait long enough for transactions to have been inserted:
            waitForTxImmutability ctx

            -- 4. Recurse until the server tells us there's nothing left to
            -- migrate:
            migrateWallet ctx src targets
      where
        endpointCreateMigrationPlan =
            Link.createMigrationPlan @'Byron src
        endpointMigrateWallet =
            Link.migrateWallet @'Byron src
        endpointListTxs =
            Link.listTransactions @'Byron src
        endpointForget =
            Link.deleteTransaction @'Byron src

        payloadCreateMigrationPlan = Json [json|{"addresses": #{targets}}|]
        payloadMigrateWallet = Json [json|
            { "passphrase": #{fixturePassphrase}
            , "addresses": #{targets}
            }|]

        forgetTxIf predicate tx
            | predicate tx =
                void $ unsafeRequest @() ctx (endpointForget tx) Empty
            | otherwise =
                pure ()

    testAddressCycling sourceWalletType mkSourceWallet targetAddressCount = do
        let title = mconcat
                [ "Migration from "
                , sourceWalletType
                , " wallet to target address count: "
                , show targetAddressCount
                , "."
                ]
        it title $ \ctx -> runResourceT $ do

            -- Restore a Byron wallet with funds, to act as a source wallet:
            sourceWallet <- mkSourceWallet ctx
            let sourceBalance =
                    view (#balance. #available . #getQuantity) sourceWallet

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = take targetAddressCount targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)

            -- Create a migration plan:
            response0 <- request @(ApiWalletMigrationPlan n) ctx
                (Link.createMigrationPlan @'Byron sourceWallet) Default
                (Json [json|{addresses: #{targetAddressIds}}|])
            verify response0
                [ expectResponseCode HTTP.status202
                , expectField #totalFee (.> Quantity 0)
                ]
            let expectedFee = getFromResponse
                    (#totalFee . #getQuantity) response0
            let balanceLeftover = getFromResponse
                    (#balanceLeftover . #ada . #getQuantity) response0

            -- Perform a migration from the source wallet to the target wallet:
            response1 <- request @[ApiTransaction n] ctx
                (Link.migrateWallet @'Byron sourceWallet)
                Default
                (Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: #{targetAddressIds}
                    }|])
            verify response1
                [ expectResponseCode HTTP.status202
                , expectField id (`shouldSatisfy` (not . null))
                ]

            -- Check that funds have become available in the target wallet:
            let expectedTargetBalance =
                    sourceBalance - expectedFee - balanceLeftover
            waitForTxImmutability ctx
            request @ApiWallet ctx
                (Link.getWallet @'Shelley targetWallet) Default Empty
                >>= flip verify
                    [ expectField
                        (#balance . #available)
                        (`shouldBe` Quantity expectedTargetBalance)
                    , expectField
                        (#balance . #total)
                        (`shouldBe` Quantity expectedTargetBalance)
                    ]

            -- Check that the source wallet has a balance of zero:
            responseFinalSourceBalance <- request @ApiByronWallet ctx
                (Link.getWallet @'Byron sourceWallet) Default Empty
            verify responseFinalSourceBalance
                [ expectField
                    (#balance . #available)
                    (`shouldBe` Quantity 0)
                , expectField
                    (#balance . #total)
                    (`shouldBe` Quantity 0)
                ]
