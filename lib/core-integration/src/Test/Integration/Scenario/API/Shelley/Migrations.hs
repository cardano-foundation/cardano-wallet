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

module Test.Integration.Scenario.API.Shelley.Migrations
    ( spec
    ) where

import Prelude

import Cardano.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Cardano.Wallet.Api.Types
    ( ApiEra (..)
    , ApiT (..)
    , ApiTransaction
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
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxStatus (..) )
import Control.Monad
    ( forM_, replicateM_, void, when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Function
    ( (&) )
import Data.Functor
    ( (<&>) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Proxy
    ( Proxy )
import Data.Quantity
    ( Quantity (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Faucet
    ( bigDustWallet, onlyDustWallet )
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
    , fixtureMultiAssetWallet
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWith
    , getFromResponse
    , icarusAddresses
    , json
    , listAddresses
    , postWallet
    , randomAddresses
    , request
    , rewardWallet
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
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
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
spec = describe "SHELLEY_MIGRATIONS" $ do

    it "SHELLEY_CREATE_MIGRATION_PLAN_01 - \
        \Can create a migration plan."
        $ \ctx -> runResourceT $ do
            sourceWallet <- fixtureWallet ctx
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Shelley sourceWallet
            response <- request @(ApiWalletMigrationPlan n) ctx ep Default
                (Json [json|{addresses: #{targetAddressIds}}|])
            verify response
                [ expectResponseCode HTTP.status202
                , expectField (#totalFee . #getQuantity)
                    (`shouldBe`
                        if _mainEra ctx >= ApiBabbage
                        then 255_100
                        else 254_900)
                , expectField (#selections)
                    ((`shouldBe` 1) . length)
                , expectField (#balanceSelected . #ada . #getQuantity)
                    (`shouldBe` 1_000_000_000_000)
                , expectField (#balanceLeftover . #ada . #getQuantity)
                    (`shouldBe` 0)
                ]

    it "SHELLEY_CREATE_MIGRATION_PLAN_02 - \
        \Cannot create plan for empty wallet."
        $ \ctx -> runResourceT $ do
            sourceWallet <- emptyWallet ctx
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Shelley sourceWallet
            response <- request @(ApiWalletMigrationPlan n) ctx ep Default
                (Json [json|{addresses: #{targetAddressIds}}|])
            verify response
                [ expectResponseCode HTTP.status403
                , expectErrorMessage
                    (errMsg403NothingToMigrate $ sourceWallet ^. walletId)
                ]

    describe "SHELLEY_CREATE_MIGRATION_PLAN_03 - \
        \Cannot create plan for Byron wallet using Shelley endpoint." $ do
        let sourceWallets =
              [ ("Random", emptyRandomWallet)
              , ("Icarus", emptyIcarusWallet)
              ]
        forM_ sourceWallets $ \(walletType, byronWallet) -> do
            let title = mconcat
                    [ "Cannot calculate Shelley migration using wallet: "
                    , walletType
                    ]
            it title $ \ctx -> runResourceT $ do
                sourceWallet <- byronWallet ctx
                targetWallet <- emptyWallet ctx
                targetAddresses <- listAddresses @n ctx targetWallet
                let targetAddressIds = targetAddresses <&>
                        (\(ApiTypes.ApiAddress addrId _ _) -> addrId)
                let ep = Link.createMigrationPlan @'Shelley sourceWallet
                result <- request
                    @(ApiWalletMigrationPlan n) ctx ep Default
                    (Json [json|{addresses: #{targetAddressIds}}|])
                verify result
                    [ expectResponseCode HTTP.status404
                    , expectErrorMessage
                        (errMsg404NoWallet $ sourceWallet ^. walletId)
                    ]

    Hspec.it "SHELLEY_CREATE_MIGRATION_PLAN_04 - \
        \Cannot create a plan for a wallet that only contains freeriders."
        $ \ctx -> runResourceT @IO $ do
            sourceWallet <- emptyWallet ctx
            srcAddrs <- map (getApiT . fst . view #id)
                <$> listAddresses @n ctx sourceWallet

            -- Add a relatively small number of freerider UTxO entries to the
            -- source wallet. (Few enough to not require more than one
            -- transaction within a complete migration plan.)
            --
            -- We assign to each UTxO entry:
            --
            --  - a fixed quantity of non-ada assets.
            --
            --  - a fixed quantity of ada that is above the minimum, but not
            --    enough to allow the entry to be included in a singleton
            --    transaction (thus making it a freerider).
            --
            -- We use '_mintSeaHorseAssets' to mint non-ada assets. Since this
            -- function doesn't know how to compute the minimum ada quantity
            -- for a token bundle, we provide a custom ada quantity here. This
            -- ada quantity is large enough to allow minting to succeed, but
            -- small enough to make the migration algorithm categorize the
            -- entry as a freerider.

            let perEntryAdaQuantity = Coin $ case _mainEra ctx of
                    e | e >= ApiAlonzo -> 3_100_000
                      | otherwise      -> 3_300_000

            let perEntryAssetCount = 10
            let batchSize = 20
            liftIO $ _mintSeaHorseAssets ctx
                perEntryAssetCount
                batchSize
                perEntryAdaQuantity
                srcAddrs
            waitForTxImmutability ctx

            -- Check that the minting indeed worked, and that the wallet isn't
            -- empty:
            request @ApiWallet ctx
                (Link.getWallet @'Shelley sourceWallet) Default Empty
                >>= flip verify
                [ expectField (#balance . #available . #getQuantity)
                    (.> 0)
                , expectField (#assets . #available . #getApiT)
                    ((.> 0) . TokenMap.size)
                ]

            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Shelley sourceWallet
            response <- request @(ApiWalletMigrationPlan n) ctx ep Default
                (Json [json|{addresses: #{targetAddressIds}}|])
            verify response
                [ expectResponseCode HTTP.status403
                , expectErrorMessage
                    (errMsg403NothingToMigrate $ sourceWallet ^. walletId)
                ]

    it "SHELLEY_CREATE_MIGRATION_PLAN_05 - \
        \Creating a plan is deterministic."
        $ \ctx -> runResourceT $ do
            sourceWallet <- fixtureWallet ctx
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Shelley sourceWallet
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
                    plan1 `shouldBe` plan2
                _ ->
                    error "Unable to compare plans."

    it "SHELLEY_CREATE_MIGRATION_PLAN_06 - \
        \Can create a migration plan for a wallet that has rewards."
        $ \ctx -> runResourceT $ do
            (sourceWallet, _sourceWalletMnemonic) <- rewardWallet ctx
            -- Check that the source wallet has the expected balance.
            request @ApiWallet ctx
                (Link.getWallet @'Shelley sourceWallet) Default Empty
                >>= flip verify
                [ expectField (#balance . #reward . #getQuantity)
                    (`shouldBe` 1_000_000_000_000)
                , expectField (#balance . #available . #getQuantity)
                    (`shouldBe`   100_000_000_000)
                , expectField (#balance . #total . #getQuantity)
                    (`shouldBe` 1_100_000_000_000)
                ]
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Shelley sourceWallet
            response <- request @(ApiWalletMigrationPlan n) ctx ep Default
                (Json [json|{addresses: #{targetAddressIds}}|])
            verify response
                [ expectResponseCode HTTP.status202
                , expectField (#totalFee . #getQuantity)
                    (`shouldBe`
                        if _mainEra ctx >= ApiBabbage
                        then 139_200
                        else 139_000)
                , expectField (#selections)
                    ((`shouldBe` 1) . length)
                , expectField (#selections)
                    ((`shouldBe` 1) . length . view #withdrawals . NE.head)
                , expectField (#selections)
                    ((`shouldBe` 1_000_000_000_000)
                        . view #getQuantity
                        . view #amount
                        . head
                        . view #withdrawals
                        . NE.head)
                , expectField (#balanceSelected . #ada . #getQuantity)
                    (`shouldBe` 1_100_000_000_000)
                , expectField (#balanceLeftover . #ada . #getQuantity)
                    (`shouldBe` 0)
                ]

    Hspec.it "SHELLEY_CREATE_MIGRATION_PLAN_07 - \
        \Can create a complete migration plan for a wallet with a large number \
        \of freerider UTxO entries, but with just enough non-freerider entries \
        \to enable the entire UTxO set to be migrated."
        $ \ctx -> runResourceT @IO $ do

            -- Create a source wallet with some pure ada entries, where each
            -- ada entry is large enough to create a singleton transaction:
            sourceWallet <- fixtureWalletWith @n ctx
                [ 100_000_000
                , 100_000_000
                ]

            -- Check that the source wallet has the expected balance and UTxO
            -- distribution:
            request @ApiWallet ctx
                (Link.getWallet @'Shelley sourceWallet) Default Empty
                >>= flip verify
                [ expectField (#balance . #reward . #getQuantity)
                    (`shouldBe` 0)
                , expectField (#balance . #available . #getQuantity)
                    (`shouldBe` 200_000_000)
                , expectField (#balance . #total . #getQuantity)
                    (`shouldBe` 200_000_000)
                ]
            let expectedSourceDistribution =
                    [(100_000_000, 2)]
            request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics @'Shelley sourceWallet) Default Empty
                >>= flip verify
                [ expectField #distribution
                    ((`shouldBe` expectedSourceDistribution)
                    . Map.toList
                    . Map.filter (> 0)
                    )
                ]

            -- Add a relatively large number of freerider UTxO entries to the
            -- source wallet.
            --
            -- We assign to each UTxO entry:
            --
            --  - a fixed token quantity of exactly one non-ada asset.
            --
            --  - a fixed quantity of ada that is above the minimum, but not
            --    enough to allow the entry to be included in a singleton
            --    transaction (thus making it a freerider).
            --
            -- We use '_mintSeaHorseAssets' to mint non-ada assets. Since this
            -- function doesn't know how to compute the minimum ada quantity
            -- for a token bundle, we provide a custom ada quantity here. This
            -- ada quantity is large enough to allow minting to succeed, but
            -- small enough to make the migration algorithm categorize the
            -- entry as a freerider.
            --
            let perEntryAdaQuantity = Coin 1_562_500
            let perEntryAssetCount = 1
            let batchSize = 20
            sourceAddresses <- take 20 . map (getApiT . fst . view #id)
                <$> listAddresses @n ctx sourceWallet
            replicateM_ 6 $ liftIO $ _mintSeaHorseAssets ctx
                perEntryAssetCount
                batchSize
                perEntryAdaQuantity
                sourceAddresses
            waitForTxImmutability ctx

            -- Check that minting was successful, and that the balance and UTxO
            -- distribution have both changed accordingly:
            let expectedBalanceAda = 387_500_000
            let expectedAssetCount = 20
            request @ApiWallet ctx
                (Link.getWallet @'Shelley sourceWallet) Default Empty
                >>= flip verify
                [ expectField (#balance . #available . #getQuantity)
                    (`shouldBe` expectedBalanceAda)
                , expectField (#balance . #total . #getQuantity)
                    (`shouldBe` expectedBalanceAda)
                , expectField (#assets . #available . #getApiT)
                    ((`shouldBe` expectedAssetCount) . TokenMap.size)
                ]
            let expectedSourceDistributionAfterMinting =
                    [ ( 10_000_000, 120)
                    , (100_000_000,   2)
                    ]
            request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics @'Shelley sourceWallet) Default Empty
                >>= flip verify
                [ expectField #distribution
                    ((`shouldBe` expectedSourceDistributionAfterMinting)
                        . Map.toList
                        . Map.filter (> 0)
                    )
                ]

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)

            -- Create a migration plan, and check that the plan is complete:
            let ep = Link.createMigrationPlan @'Shelley sourceWallet
            request @(ApiWalletMigrationPlan n) ctx ep Default
                (Json [json|{addresses: #{targetAddressIds}}|])
                >>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#selections)
                    ((`shouldBe` 2) . length)
                , expectField id
                    ((`shouldBe` 122) . apiPlanTotalInputCount)
                , expectField id
                    ((`shouldBe` 2) . apiPlanTotalOutputCount)
                , expectField (#balanceSelected . #ada . #getQuantity)
                    (`shouldBe` expectedBalanceAda)
                , expectField (#balanceSelected . #assets . #getApiT)
                    ((`shouldBe` expectedAssetCount) . TokenMap.size)
                , expectField (#balanceLeftover . #ada . #getQuantity)
                    (`shouldBe` 0)
                , expectField (#balanceLeftover . #assets . #getApiT)
                    ((`shouldBe` 0) . TokenMap.size)
                ]

    Hspec.it "SHELLEY_CREATE_MIGRATION_PLAN_08 - \
        \Can create a partial migration plan for a wallet with a large number \
        \of freerider UTxO entries, but with not quite enough non-freerider \
        \entries to enable the entire UTxO set to be migrated."
        $ \ctx -> runResourceT @IO $ do

            -- Create a source wallet with just one pure ada entry that is
            -- large enough to create a singleton transaction:
            sourceWallet <- fixtureWalletWith @n ctx [ 100_000_000 ]

            -- Check that the source wallet has the expected balance and UTxO
            -- distribution:
            request @ApiWallet ctx
                (Link.getWallet @'Shelley sourceWallet) Default Empty
                >>= flip verify
                [ expectField (#balance . #reward . #getQuantity)
                    (`shouldBe` 0)
                , expectField (#balance . #available . #getQuantity)
                    (`shouldBe` 100_000_000)
                , expectField (#balance . #total . #getQuantity)
                    (`shouldBe` 100_000_000)
                ]
            let expectedSourceDistribution =
                    [(100_000_000, 1)]
            request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics @'Shelley sourceWallet) Default Empty
                >>= flip verify
                [ expectField #distribution
                    ((`shouldBe` expectedSourceDistribution)
                    . Map.toList
                    . Map.filter (> 0)
                    )
                ]

            -- Add a relatively large number of freerider UTxO entries to the
            -- source wallet.
            --
            -- We assign to each UTxO entry:
            --
            --  - a fixed token quantity of exactly one non-ada asset.
            --
            --  - a fixed quantity of ada that is above the minimum, but not
            --    enough to allow the entry to be included in a singleton
            --    transaction (thus making it a freerider).
            --
            -- We use '_mintSeaHorseAssets' to mint non-ada assets. Since this
            -- function doesn't know how to compute the minimum ada quantity
            -- for a token bundle, we provide a custom ada quantity here. This
            -- ada quantity is large enough to allow minting to succeed, but
            -- small enough to make the migration algorithm categorize the
            -- entry as a freerider.
            --
            let perEntryAdaQuantity = Coin 1_562_500
            let perEntryAssetCount = 1
            let batchSize = 20
            sourceAddresses <- take 20 . map (getApiT . fst . view #id)
                <$> listAddresses @n ctx sourceWallet
            replicateM_ 6 $ liftIO $ _mintSeaHorseAssets ctx
                perEntryAssetCount
                batchSize
                perEntryAdaQuantity
                sourceAddresses
            waitForTxImmutability ctx

            -- Check that minting was successful, and that the balance and UTxO
            -- distribution have both changed accordingly:
            let expectedBalanceAda = 287_500_000
            let expectedAssetCount = 20
            request @ApiWallet ctx
                (Link.getWallet @'Shelley sourceWallet) Default Empty
                >>= flip verify
                [ expectField (#balance . #available . #getQuantity)
                    (`shouldBe` expectedBalanceAda)
                , expectField (#balance . #total . #getQuantity)
                    (`shouldBe` expectedBalanceAda)
                , expectField (#assets . #available . #getApiT)
                    ((`shouldBe` expectedAssetCount) . TokenMap.size)
                ]
            let expectedSourceDistributionAfterMinting =
                    [ ( 10_000_000, 120)
                    , (100_000_000,   1)
                    ]
            request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics @'Shelley sourceWallet) Default Empty
                >>= flip verify
                [ expectField #distribution
                    ((`shouldBe` expectedSourceDistributionAfterMinting)
                    . Map.toList
                    . Map.filter (> 0)
                    )
                ]

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)

            -- Create a migration plan, and check that the plan is only
            -- partially complete:
            let ep = Link.createMigrationPlan @'Shelley sourceWallet
            request @(ApiWalletMigrationPlan n) ctx ep Default
                (Json [json|{addresses: #{targetAddressIds}}|])
                >>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#selections)
                    ((`shouldBe` 1) . length)
                , expectField id
                    ((`shouldBe` 102) . apiPlanTotalInputCount)
                , expectField id
                    ((`shouldBe` 1) . apiPlanTotalOutputCount)
                , expectField (#balanceSelected . #ada . #getQuantity)
                    (`shouldBe` 257_812_500)
                , expectField (#balanceLeftover . #ada . #getQuantity)
                    (`shouldBe`  29_687_500)
                , expectField (#balanceSelected . #assets . #getApiT)
                    ((.> 0) . TokenMap.size)
                , expectField (#balanceLeftover . #assets . #getApiT)
                    ((.> 0) . TokenMap.size)
                ]

    describe "SHELLEY_MIGRATE_01 - \
        \After a migration operation successfully completes, the correct \
        \amounts eventually become available in the target wallet for an \
        \arbitrary number of specified addresses, and the balance of the \
        \source wallet is completely depleted."
        $ do
            testAddressCycling  1
            testAddressCycling  3
            testAddressCycling 10

    Hspec.it "SHELLEY_MIGRATE_02 - \
        \Can migrate a large wallet requiring more than one transaction."
        $ \ctx -> runResourceT @IO $ do

        -- Create a large source wallet from which funds will be migrated:
        sourceWallet <- unsafeResponse <$> postWallet ctx
            (Json [json|{
                "name": "Big Shelley Wallet",
                "mnemonic_sentence": #{mnemonicToText bigDustWallet},
                "passphrase": #{fixturePassphrase}
            }|])
        sourceBalance <- eventually "Source wallet balance is correct." $ do
            response <- request @ApiWallet ctx
                (Link.getWallet @'Shelley sourceWallet) Default Empty
            verify response
                [ expectField (#balance . #available . #getQuantity)
                    (`shouldBe` 10_000_100_000_000)
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
            (Link.createMigrationPlan @'Shelley sourceWallet) Default
            (Json [json|{addresses: #{targetAddressIds}}|])
        verify responsePlan
            [ expectResponseCode HTTP.status202
            , expectField
                (#totalFee . #getQuantity)
                (`shouldBe`
                    if _mainEra ctx >= ApiBabbage
                    then 3_120_200
                    else 3_119_800)
            , expectField
                (#selections)
                ((`shouldBe` 2) . length)
            , expectField
                (#balanceLeftover . #ada . #getQuantity)
                (`shouldBe` 0)
            , expectField
                (#balanceSelected . #ada . #getQuantity)
                (`shouldBe` 10_000_100_000_000)
            ]
        let expectedFee = getFromResponse
                (#totalFee . #getQuantity) responsePlan
        let balanceLeftover = getFromResponse
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

        -- Check that funds become available in the target wallet:
        let expectedTargetBalance =
                sourceBalance - balanceLeftover - expectedFee
        response <- request @ApiWallet ctx
            (Link.getWallet @'Shelley targetWallet) Default Empty
        verify response
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
                ((`shouldBe` (Just 2)) . Map.lookup 10_000_000_000_000)
            ]

        -- Check that the source wallet has the expected leftover balance:
        responseFinalSourceBalance <- request @ApiWallet ctx
            (Link.getWallet @'Shelley sourceWallet) Default Empty
        verify responseFinalSourceBalance
            [ expectResponseCode HTTP.status200
            , expectField (#balance . #available)
                (`shouldBe` Quantity 0)
            , expectField (#balance . #total)
                (`shouldBe` Quantity 0)
            ]

    it "SHELLEY_MIGRATE_03 - \
        \Migrating an empty wallet should fail."
        $ \ctx -> runResourceT $ do
            sourceWallet <- emptyWallet ctx
            let sourceWalletId = sourceWallet ^. walletId
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)
            let ep = Link.migrateWallet @'Shelley sourceWallet
            response <- request @[ApiTransaction n] ctx ep Default $
                    Json [json|
                        { passphrase: #{fixturePassphrase}
                        , addresses: #{targetAddressIds}
                        }|]
            verify response
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate sourceWalletId)
                ]

    Hspec.it "SHELLEY_MIGRATE_04 - \
        \Actual fee for migration is identical to predicted fee."
        $ \ctx -> runResourceT @IO $ do

            let feeExpected =
                    if _mainEra ctx >= ApiBabbage
                    then 255_100
                    else 254_900


            -- Restore a source wallet with funds:
            sourceWallet <- fixtureWallet ctx

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)

            -- Create a migration plan:
            let endpointPlan = (Link.createMigrationPlan @'Shelley sourceWallet)
            responsePlan <- request @(ApiWalletMigrationPlan n)
                ctx endpointPlan Default $
                Json [json|{addresses: #{targetAddressIds}}|]
            -- Verify the fee is as expected:
            verify responsePlan
                [ expectResponseCode HTTP.status202
                , expectField #totalFee (`shouldBe` Quantity feeExpected)
                , expectField #selections ((`shouldBe` 1) . length)
                ]

            -- Perform a migration:
            let endpointMigrate = Link.migrateWallet @'Shelley sourceWallet
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

    it "SHELLEY_MIGRATE_05 - \
        \Migration fails if the wrong passphrase is supplied."
        $ \ctx -> runResourceT $ do

            -- Restore a Shelley wallet with funds, to act as a source wallet:
            sourceWallet <- fixtureWallet ctx

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)

            -- Attempt to perform a migration:
            response <- request @[ApiTransaction n] ctx
                (Link.migrateWallet @'Shelley sourceWallet)
                Default
                (Json [json|
                    { passphrase: "not-the-right-passphrase"
                    , addresses: #{targetAddressIds}
                    }|])
            verify response
                [ expectResponseCode HTTP.status403
                , expectErrorMessage errMsg403WrongPass
                ]

    it "SHELLEY_MIGRATE_06 - \
        \It's possible to migrate to any valid address."
        $ \ctx -> runResourceT $ do

            -- Create a Shelley address:
            wShelley <- emptyWallet ctx
            addrs <- listAddresses @n ctx wShelley
            let addrShelley = (addrs !! 1) ^. #id

            -- Create an Icarus address:
            addrIcarus <- liftIO $ encodeAddress @n . head . icarusAddresses @n
                . entropyToMnemonic @15 <$> genEntropy

            -- Create a Byron address:
            addrByron <- liftIO $ encodeAddress @n . head . randomAddresses @n
                . entropyToMnemonic @12 <$> genEntropy

            -- Create a source wallet:
            sourceWallet <- emptyWallet ctx

            -- Initiate a migration to all address types:
            response <- request @[ApiTransaction n] ctx
                (Link.migrateWallet @'Shelley sourceWallet) Default
                (Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: [#{addrShelley}, #{addrIcarus}, #{addrByron}]
                    }|])
            verify response
                [ expectResponseCode HTTP.status403
                , expectErrorMessage
                    (errMsg403NothingToMigrate (sourceWallet ^. walletId))
                ]

    it "SHELLEY_MIGRATE_07 - \
        \Including an invalidly-formatted passphrase results in a parser error."
        $ \ctx -> runResourceT $ do
            sourceWallet <- emptyWallet ctx
            response <- request @[ApiTransaction n] ctx
                (Link.migrateWallet @'Shelley sourceWallet) Default
                (NonJson "{passphrase:,}")
            verify response
                [ expectResponseCode HTTP.status400
                , expectErrorMessage errMsg400ParseError
                ]

    Hspec.it "SHELLEY_MIGRATE_08 - \
        \It's possible to migrate a wallet with many small ada quantities, \
        \provided that the total balance is significantly greater than the \
        \minimum ada quantity for an output."
        $ \ctx -> runResourceT @IO $ do

            -- Create a source wallet with many small ada quantities:
            sourceWallet <- unsafeResponse <$> postWallet ctx
                (Json [json|{
                    "name": "Shelley Wallet",
                    "mnemonic_sentence": #{mnemonicToText onlyDustWallet},
                    "passphrase": #{fixturePassphrase}
                }|])
            sourceBalance <- eventually "Source wallet balance is correct." $ do
                response <- request @ApiWallet ctx
                    (Link.getWallet @'Shelley sourceWallet) Default Empty
                verify response
                    [ expectField (#balance . #available . #getQuantity)
                        (`shouldBe` 43_000_000)
                    , expectField (#balance . #total . #getQuantity)
                        (`shouldBe` 43_000_000)
                    ]
                pure $ getFromResponse (#balance. #available . #getQuantity)
                    response

            -- Analyse the source wallet's UTxO distribution:
            let expectedSourceDistribution =
                    [ (  1_000_000, 3)
                    , ( 10_000_000, 6)
                    , (100_000_000, 1)
                    ]
            responseSourceDistribution <- request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics @'Shelley sourceWallet) Default Empty
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

            -- Compute the expected migration plan:
            let feeExpected =
                    if _mainEra ctx >= ApiBabbage
                    then 254_700
                    else 254_500
            responsePlan <- request @(ApiWalletMigrationPlan n) ctx
                (Link.createMigrationPlan @'Shelley sourceWallet) Default
                (Json [json|{addresses: #{targetAddressIds}}|])
            verify responsePlan
                [ expectResponseCode HTTP.status202
                , expectField #totalFee (`shouldBe` Quantity feeExpected)
                , expectField #selections ((`shouldBe` 1) . length)
                ]

            -- Perform the migration:
            let ep = Link.migrateWallet @'Shelley sourceWallet
            responseMigrate <- request @[ApiTransaction n] ctx ep Default $
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

            waitForTxImmutability ctx

            -- Check that funds become available in the target wallet:
            let expectedBalance = sourceBalance - feeExpected
            request @ApiWallet ctx
                (Link.getWallet @'Shelley targetWallet)
                Default
                Empty >>= flip verify
                [ expectField
                    (#balance . #available)
                    ( `shouldBe` Quantity expectedBalance)
                , expectField
                    (#balance . #total)
                    ( `shouldBe` Quantity expectedBalance)
                ]

            -- Analyse the target wallet's UTxO distribution:
            let expectedTargetDistribution = [(100_000_000, 1)]
            responseTargetDistribution <- request @ApiUtxoStatistics ctx
                (Link.getUTxOsStatistics @'Shelley targetWallet) Default Empty
            verify responseTargetDistribution
                [ expectField #distribution
                    ((`shouldBe` expectedTargetDistribution)
                    . Map.toList
                    . Map.filter (> 0)
                    )
                ]

    Hspec.it "SHELLEY_MIGRATE_09 - \
        \Can migrate a wallet that has rewards."
        $ \ctx -> runResourceT @IO $ do

            -- Create a source wallet with rewards:
            (sourceWallet, _sourceWalletMnemonic) <- rewardWallet ctx

            -- Check that the source wallet has the expected balance:
            let expectedAdaBalanceAvailable =   100_000_000_000
            let expectedAdaBalanceReward    = 1_000_000_000_000
            let expectedAdaBalanceTotal     = 1_100_000_000_000
            request @ApiWallet ctx
                (Link.getWallet @'Shelley sourceWallet) Default Empty
                >>= flip verify
                [ expectField (#balance . #available . #getQuantity)
                    (`shouldBe` expectedAdaBalanceAvailable)
                , expectField (#balance . #reward . #getQuantity)
                    (`shouldBe` expectedAdaBalanceReward)
                , expectField (#balance . #total . #getQuantity)
                    (`shouldBe` expectedAdaBalanceTotal)
                ]

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)

            -- Perform a migration:
            let ep = Link.migrateWallet @'Shelley sourceWallet
            responseMigrate <- request @[ApiTransaction n] ctx ep Default $
                Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: #{targetAddressIds}
                    }|]

            -- Verify the fee is as expected:
            let expectedFee =
                    if _mainEra ctx >= ApiBabbage
                    then 139_200
                    else 139_000
            verify responseMigrate
                [ expectResponseCode HTTP.status202
                , expectField id ((`shouldBe` 1) . length)
                , expectField id
                    $ (`shouldBe` expectedFee)
                    . sum
                    . fmap apiTransactionFee
                ]

            waitForTxImmutability ctx

            -- Check that funds become available in the target wallet:
            let expectedTargetBalance = expectedAdaBalanceTotal - expectedFee
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

            -- Check that the source wallet has been depleted:
            request @ApiWallet ctx
                (Link.getWallet @'Shelley sourceWallet) Default Empty
                >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField
                    (#balance . #available)
                    (`shouldBe` Quantity 0)
                , expectField
                    (#balance . #total)
                    (`shouldBe` Quantity 0)
                ]

    Hspec.it "SHELLEY_MIGRATE_MULTI_ASSET_01 - \
        \Can migrate a multi-asset wallet."
        $ \ctx -> runResourceT @IO $ do

            -- Restore a source wallet with funds:
            sourceWallet <- fixtureMultiAssetWallet ctx

            -- Wait for the source wallet balance to be correct:
            let expectedAdaBalance = 40_000_000
            sourceBalance <- eventually "Source wallet balance is correct." $ do
                response <- request @ApiWallet ctx
                    (Link.getWallet @'Shelley sourceWallet) Default Empty
                verify response
                    [ expectField (#balance . #available . #getQuantity)
                        (`shouldBe` expectedAdaBalance)
                    , expectField (#balance . #total . #getQuantity)
                        (`shouldBe` expectedAdaBalance)
                    , expectField (#assets . #available . #getApiT)
                        ((`shouldBe` 8) . TokenMap.size)
                    , expectField (#assets . #total . #getApiT)
                        ((`shouldBe` 8) . TokenMap.size)
                    ]
                let balanceAda = response
                        & getFromResponse (#balance . #available . #getQuantity)
                        & fromIntegral
                        & Coin
                let balanceAssets = response
                        & getFromResponse (#assets . #available . #getApiT)
                pure $ TokenBundle balanceAda balanceAssets

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)

            -- Create a migration plan:
            let endpointPlan = (Link.createMigrationPlan @'Shelley sourceWallet)
            responsePlan <- request @(ApiWalletMigrationPlan n)
                ctx endpointPlan Default $
                Json [json|{addresses: #{targetAddressIds}}|]

            -- Verify the plan is as expected:
            let expectedFee =
                    if _mainEra ctx >= ApiBabbage
                    then 191_000
                    else 190_800
            verify responsePlan
                [ expectResponseCode HTTP.status202
                , expectField (#totalFee . #getQuantity)
                    (`shouldBe` expectedFee)
                , expectField (#selections)
                    ((`shouldBe` 1) . length)
                , expectField id
                    ((`shouldBe` 3) . apiPlanTotalInputCount)
                , expectField id
                    ((`shouldBe` 1) . apiPlanTotalOutputCount)
                , expectField (#balanceSelected . #ada)
                    (`shouldBe` coinToQuantity (view #coin sourceBalance))
                , expectField (#balanceLeftover . #ada . #getQuantity)
                    (`shouldBe` 0)
                , expectField (#balanceSelected . #assets . #getApiT)
                    (`shouldBe` view #tokens sourceBalance)
                , expectField (#balanceLeftover . #assets . #getApiT)
                    (`shouldSatisfy` TokenMap.isEmpty)
                ]

            -- Perform a migration:
            let endpointMigrate = Link.migrateWallet @'Shelley sourceWallet
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
                    $ (`shouldBe` expectedFee)
                    . fromIntegral
                    . sum
                    . fmap apiTransactionFee
                ]

            waitForTxImmutability ctx

            -- Check that funds become available in the target wallet:
            let expectedTargetBalance = expectedAdaBalance - expectedFee
            request @ApiWallet ctx
                (Link.getWallet @'Shelley targetWallet) Default Empty
                >>= flip verify
                [ expectField
                    (#balance . #available)
                    (`shouldBe` Quantity expectedTargetBalance)
                , expectField
                    (#balance . #total)
                    (`shouldBe` Quantity expectedTargetBalance)
                , expectField
                    (#assets . #available . #getApiT)
                    (`shouldBe` view #tokens sourceBalance)
                , expectField
                    (#assets . #total . #getApiT)
                    (`shouldBe` view #tokens sourceBalance)
                ]

            -- Check that the source wallet has been depleted:
            responseFinalSourceBalance <- request @ApiWallet ctx
                (Link.getWallet @'Shelley sourceWallet) Default Empty
            verify responseFinalSourceBalance
                [ expectResponseCode HTTP.status200
                , expectField
                    (#balance . #available)
                    (`shouldBe` Quantity 0)
                , expectField
                    (#balance . #total)
                    (`shouldBe` Quantity 0)
                , expectField
                    (#assets . #available . #getApiT)
                    (`shouldSatisfy` TokenMap.isEmpty)
                , expectField
                    (#assets . #total . #getApiT)
                    (`shouldSatisfy` TokenMap.isEmpty)
                ]
  where
    -- Compute the fee associated with an API transaction.
    apiTransactionFee :: ApiTransaction n -> Natural
    apiTransactionFee = view (#fee . #getQuantity)

    migrateWallet
        :: Context
        -> ApiWallet
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
            Link.createMigrationPlan @'Shelley src
        endpointMigrateWallet =
            Link.migrateWallet @'Shelley src
        endpointListTxs =
            Link.listTransactions @'Shelley src
        endpointForget =
            Link.deleteTransaction @'Shelley src

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

    testAddressCycling targetAddressCount = do
        let title = mconcat
                [ "Migration from Shelley wallet to target address count: "
                , show targetAddressCount
                , "."
                ]
        it title $ \ctx -> runResourceT $ do

            -- Restore a Shelley wallet with funds, to act as a source wallet:
            sourceWallet <- fixtureWallet ctx
            let sourceBalance =
                    view (#balance. #available . #getQuantity) sourceWallet

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds = take targetAddressCount targetAddresses <&>
                    (\(ApiTypes.ApiAddress addrId _ _) -> addrId)

            -- Create a migration plan:
            response0 <- request @(ApiWalletMigrationPlan n) ctx
                (Link.createMigrationPlan @'Shelley sourceWallet) Default
                (Json [json|{addresses: #{targetAddressIds}}|])
            verify response0
                [ expectResponseCode HTTP.status202
                , expectField #totalFee (.> Quantity 0)
                ]
            let expectedFee =
                    getFromResponse (#totalFee . #getQuantity) response0

            -- Perform a migration from the source wallet to the target wallet:
            response1 <- request @[ApiTransaction n] ctx
                (Link.migrateWallet @'Shelley sourceWallet)
                Default
                (Json [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: #{targetAddressIds}
                    }|])
            verify response1
                [ expectResponseCode HTTP.status202
                , expectField id (`shouldSatisfy` (not . null))
                ]

            waitForTxImmutability ctx

            -- Check that funds have become available in the target wallet:
            let expectedTargetBalance = sourceBalance - expectedFee
            response2 <- request @ApiWallet ctx
                (Link.getWallet @'Shelley targetWallet) Default Empty
            verify response2
                [ expectField
                    (#balance . #available)
                    (`shouldBe` Quantity expectedTargetBalance)
                , expectField
                    (#balance . #total)
                    (`shouldBe` Quantity expectedTargetBalance)
                ]

            -- Check that the source wallet has a balance of zero:
            responseFinalSourceBalance <- request @ApiWallet ctx
                (Link.getWallet @'Shelley sourceWallet) Default Empty
            verify responseFinalSourceBalance
                [ expectField
                    (#balance . #available)
                    (`shouldBe` Quantity 0)
                , expectField
                    (#balance . #total)
                    (`shouldBe` Quantity 0)
                ]

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

apiPlanTotalInputCount :: ApiWalletMigrationPlan n -> Int
apiPlanTotalInputCount p =
    F.sum (length . view #inputs <$> view #selections p)

apiPlanTotalOutputCount :: ApiWalletMigrationPlan n -> Int
apiPlanTotalOutputCount p =
    F.sum (length . view #outputs <$> view #selections p)

coinToQuantity :: Coin -> Quantity "lovelace" Natural
coinToQuantity = Quantity . fromIntegral . unCoin
