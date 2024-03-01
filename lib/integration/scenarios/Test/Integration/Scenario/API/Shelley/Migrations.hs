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

import Cardano.Mnemonic.Extended
    ( someMnemonicToWords
    )
import Cardano.Wallet.Address.Encoding
    ( encodeAddress
    )
import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiEra (..)
    , ApiT (..)
    , ApiTransaction
    , ApiUtxoStatistics
    , ApiWallet
    , ApiWalletMigrationPlan (..)
    , WalletStyle (..)
    , apiAddress
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount (ApiAmount)
    )
import Cardano.Wallet.Faucet
    ( Faucet (..)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( unAddress
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( TxStatus (..)
    )
import Control.Monad
    ( forM_
    , replicateM_
    , void
    , when
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.Trans.Resource
    ( runResourceT
    )
import Data.Function
    ( (&)
    )
import Data.Functor
    ( (<&>)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import GHC.Exts
    ( IsList (toList)
    )
import Numeric.Natural
    ( Natural
    )
import Test.Hspec
    ( SpecWith
    , describe
    )
import Test.Hspec.Expectations.Lifted
    ( shouldBe
    , shouldSatisfy
    )
import Test.Hspec.Extra
    ( it
    )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , emptyIcarusWallet
    , emptyRandomWallet
    , emptyWallet
    , eventually
    , eventuallyReport
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
    , noConway
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
    , (.>=)
    )
import Test.Integration.Framework.TestData
    ( errMsg403NothingToMigrate
    , errMsg403WrongPass
    , errMsg404NoWallet
    )
import Text.Pretty.Simple
    ( pShowNoColor
    )

import qualified Cardano.Address as CA
import qualified Cardano.Faucet.Mnemonics as Mnemonics
import qualified Cardano.Wallet.Api.Link as Link
import qualified Cardano.Wallet.Api.Types as ApiTypes
import qualified Cardano.Wallet.Api.Types.Amount as ApiAmount
import qualified Cardano.Wallet.Api.Types.WalletAssets as ApiWalletAssets
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy as TL
import qualified Network.HTTP.Types.Status as HTTP
import qualified Test.Hspec as Hspec

spec :: forall n. HasSNetworkId n => SpecWith Context
spec = describe "SHELLEY_MIGRATIONS" $ do
    it
        "SHELLEY_CREATE_MIGRATION_PLAN_01 - \
        \Can create a migration plan."
        $ \ctx -> runResourceT $ do
            sourceWallet <- fixtureWallet ctx
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds =
                    targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Shelley sourceWallet
            response <-
                request @(ApiWalletMigrationPlan n)
                    ctx
                    ep
                    Default
                    (Json [json|{addresses: #{targetAddressIds}}|])
            verify
                response
                [ expectResponseCode HTTP.status202
                , expectField
                    (#totalFee . #toNatural)
                    ( `shouldBe`
                        if _mainEra ctx >= ApiBabbage
                            then 255_100
                            else 254_900
                    )
                , expectField
                    (#selections)
                    ((`shouldBe` 1) . length)
                , expectField
                    (#balanceSelected . #ada . #toNatural)
                    (`shouldBe` 1_000_000_000_000)
                , expectField
                    (#balanceLeftover . #ada . #toNatural)
                    (`shouldBe` 0)
                ]

    it
        "SHELLEY_CREATE_MIGRATION_PLAN_02 - \
        \Cannot create plan for empty wallet."
        $ \ctx -> runResourceT $ do
            sourceWallet <- emptyWallet ctx
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds =
                    targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Shelley sourceWallet
            response <-
                request @(ApiWalletMigrationPlan n)
                    ctx
                    ep
                    Default
                    (Json [json|{addresses: #{targetAddressIds}}|])
            verify
                response
                [ expectResponseCode HTTP.status403
                , expectErrorMessage
                    (errMsg403NothingToMigrate $ sourceWallet ^. walletId)
                ]

    describe
        "SHELLEY_CREATE_MIGRATION_PLAN_03 - \
        \Cannot create plan for Byron wallet using Shelley endpoint."
        $ do
            let sourceWallets =
                    [ ("Random", emptyRandomWallet)
                    , ("Icarus", emptyIcarusWallet)
                    ]
            forM_ sourceWallets $ \(walletType, byronWallet) -> do
                let title =
                        mconcat
                            [ "Cannot calculate Shelley migration using wallet: "
                            , walletType
                            ]
                it title $ \ctx -> runResourceT $ do
                    sourceWallet <- byronWallet ctx
                    targetWallet <- emptyWallet ctx
                    targetAddresses <- listAddresses @n ctx targetWallet
                    let targetAddressIds =
                            targetAddresses
                                <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)
                    let ep = Link.createMigrationPlan @'Shelley sourceWallet
                    result <-
                        request
                            @(ApiWalletMigrationPlan n)
                            ctx
                            ep
                            Default
                            (Json [json|{addresses: #{targetAddressIds}}|])
                    verify
                        result
                        [ expectResponseCode HTTP.status404
                        , expectErrorMessage
                            (errMsg404NoWallet $ sourceWallet ^. walletId)
                        ]

    Hspec.it
        "SHELLEY_CREATE_MIGRATION_PLAN_04 - \
        \Cannot create a plan for a wallet that only contains freeriders."
        $ \ctx -> runResourceT @IO $ do
            noConway ctx "migration accepted"
            sourceWallet <- emptyWallet ctx
            srcAddrs <-
                map (CA.unsafeMkAddress . unAddress . apiAddress . view #id)
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
                    e
                        | e == ApiAlonzo -> 3_100_000
                        | e == ApiBabbage -> 2_500_000
                        | otherwise -> 3_300_000

            let perEntryAssetCount = 10
            let batchSize = 20
            liftIO
                $ _mintSeaHorseAssets
                    ctx
                    perEntryAssetCount
                    batchSize
                    perEntryAdaQuantity
                    srcAddrs
            waitForTxImmutability ctx

            -- Check that the minting indeed worked, and that the wallet isn't
            -- empty:
            request @ApiWallet
                ctx
                (Link.getWallet @'Shelley sourceWallet)
                Default
                Empty
                >>= flip
                    verify
                    [ expectField
                        (#balance . #available . #toNatural)
                        (.> 0)
                    , expectField
                        (#assets . #available)
                        ((.> 0) . length . toList)
                    ]

            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds =
                    targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Shelley sourceWallet
            response <-
                request @(ApiWalletMigrationPlan n)
                    ctx
                    ep
                    Default
                    (Json [json|{addresses: #{targetAddressIds}}|])
            verify
                response
                [ expectResponseCode HTTP.status403
                , expectErrorMessage
                    (errMsg403NothingToMigrate $ sourceWallet ^. walletId)
                ]

    it
        "SHELLEY_CREATE_MIGRATION_PLAN_05 - \
        \Creating a plan is deterministic."
        $ \ctx -> runResourceT $ do
            sourceWallet <- fixtureWallet ctx
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds =
                    targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Shelley sourceWallet
            response1 <-
                request @(ApiWalletMigrationPlan n)
                    ctx
                    ep
                    Default
                    (Json [json|{addresses: #{targetAddressIds}}|])
            response2 <-
                request @(ApiWalletMigrationPlan n)
                    ctx
                    ep
                    Default
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

    it
        "SHELLEY_CREATE_MIGRATION_PLAN_06 - \
        \Can create a migration plan for a wallet that has rewards."
        $ \ctx -> runResourceT $ do
            (sourceWallet, _sourceWalletMnemonic) <- rewardWallet ctx
            request @ApiWallet
                ctx
                (Link.getWallet @'Shelley sourceWallet)
                Default
                Empty
                >>= flip
                    verify
                    [ expectField
                        (#balance . #reward . #toNatural)
                        (.> 0)
                    , expectField
                        (#balance . #available . #toNatural)
                        (.> 0)
                    , expectField
                        (#balance . #total . #toNatural)
                        (.> 0)
                    ]
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds =
                    targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)
            let ep = Link.createMigrationPlan @'Shelley sourceWallet
            response <-
                request @(ApiWalletMigrationPlan n)
                    ctx
                    ep
                    Default
                    (Json [json|{addresses: #{targetAddressIds}}|])
            verify
                response
                [ expectResponseCode HTTP.status202
                , expectField
                    (#totalFee . #toNatural)
                    ( `shouldBe`
                        if _mainEra ctx >= ApiBabbage
                            then 139_200
                            else 139_000
                    )
                , expectField
                    (#selections)
                    ((`shouldBe` 1) . length)
                , expectField
                    (#selections)
                    ((`shouldBe` 1) . length . view #withdrawals . NE.head)
                , expectField
                    (#selections)
                    ( (.> 0)
                        . view #toNatural
                        . view #amount
                        . head
                        . view #withdrawals
                        . NE.head
                    )
                , expectField
                    (#balanceSelected . #ada . #toNatural)
                    (.> (sourceWallet ^. (#balance . #available . #toNatural)))
                , expectField
                    (#balanceLeftover . #ada . #toNatural)
                    (`shouldBe` 0)
                ]

    Hspec.it
        "SHELLEY_CREATE_MIGRATION_PLAN_07 - \
        \Can create a complete migration plan for a wallet with a large number \
        \of freerider UTxO entries, but with just enough non-freerider entries \
        \to enable the entire UTxO set to be migrated."
        $ \ctx -> runResourceT @IO $ do
            -- Create a source wallet with some pure ada entries, where each
            -- ada entry is large enough to create a singleton transaction:
            sourceWallet <-
                fixtureWalletWith @n
                    ctx
                    [ 100_000_000
                    , 100_000_000
                    ]

            -- Check that the source wallet has the expected balance and UTxO
            -- distribution:
            request @ApiWallet
                ctx
                (Link.getWallet @'Shelley sourceWallet)
                Default
                Empty
                >>= flip
                    verify
                    [ expectField
                        (#balance . #reward . #toNatural)
                        (`shouldBe` 0)
                    , expectField
                        (#balance . #available . #toNatural)
                        (`shouldBe` 200_000_000)
                    , expectField
                        (#balance . #total . #toNatural)
                        (`shouldBe` 200_000_000)
                    ]
            let expectedSourceDistribution =
                    [(100_000_000, 2)]
            request @ApiUtxoStatistics
                ctx
                (Link.getUTxOsStatistics @'Shelley sourceWallet)
                Default
                Empty
                >>= flip
                    verify
                    [ expectField
                        #distribution
                        ( (`shouldBe` expectedSourceDistribution)
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
            sourceAddresses <-
                take 20 . map (CA.unsafeMkAddress . unAddress . apiAddress . view #id)
                    <$> listAddresses @n ctx sourceWallet
            replicateM_ 6
                $ liftIO
                $ _mintSeaHorseAssets
                    ctx
                    perEntryAssetCount
                    batchSize
                    perEntryAdaQuantity
                    sourceAddresses
            waitForTxImmutability ctx

            -- Check that minting was successful, and that the balance and UTxO
            -- distribution have both changed accordingly:
            let expectedBalanceAda = 387_500_000
            let expectedAssetCount = 20
            request @ApiWallet
                ctx
                (Link.getWallet @'Shelley sourceWallet)
                Default
                Empty
                >>= flip
                    verify
                    [ expectField
                        (#balance . #available . #toNatural)
                        (`shouldBe` expectedBalanceAda)
                    , expectField
                        (#balance . #total . #toNatural)
                        (`shouldBe` expectedBalanceAda)
                    , expectField
                        (#assets . #available)
                        ((`shouldBe` expectedAssetCount) . length . toList)
                    ]
            let expectedSourceDistributionAfterMinting =
                    [ (10_000_000, 120)
                    , (100_000_000, 2)
                    ]
            request @ApiUtxoStatistics
                ctx
                (Link.getUTxOsStatistics @'Shelley sourceWallet)
                Default
                Empty
                >>= flip
                    verify
                    [ expectField
                        #distribution
                        ( (`shouldBe` expectedSourceDistributionAfterMinting)
                            . Map.toList
                            . Map.filter (> 0)
                        )
                    ]

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds =
                    targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)

            -- Create a migration plan, and check that the plan is complete:
            let ep = Link.createMigrationPlan @'Shelley sourceWallet
            request @(ApiWalletMigrationPlan n)
                ctx
                ep
                Default
                (Json [json|{addresses: #{targetAddressIds}}|])
                >>= flip
                    verify
                    [ expectResponseCode HTTP.status202
                    , expectField
                        (#selections)
                        ((`shouldBe` 2) . length)
                    , expectField
                        id
                        ((`shouldBe` 122) . apiPlanTotalInputCount)
                    , expectField
                        id
                        ((`shouldBe` 2) . apiPlanTotalOutputCount)
                    , expectField
                        (#balanceSelected . #ada . #toNatural)
                        (`shouldBe` expectedBalanceAda)
                    , expectField
                        (#balanceSelected . #assets)
                        ((`shouldBe` expectedAssetCount) . length . toList)
                    , expectField
                        (#balanceLeftover . #ada . #toNatural)
                        (`shouldBe` 0)
                    , expectField
                        (#balanceLeftover . #assets)
                        ((`shouldBe` 0) . length . toList)
                    ]

    Hspec.it
        "SHELLEY_CREATE_MIGRATION_PLAN_08 - \
        \Can create a partial migration plan for a wallet with a large number \
        \of freerider UTxO entries, but with not quite enough non-freerider \
        \entries to enable the entire UTxO set to be migrated."
        $ \ctx -> runResourceT @IO $ do
            -- Create a source wallet with just one pure ada entry that is
            -- large enough to create a singleton transaction:
            sourceWallet <- fixtureWalletWith @n ctx [100_000_000]

            -- Check that the source wallet has the expected balance and UTxO
            -- distribution:
            request @ApiWallet
                ctx
                (Link.getWallet @'Shelley sourceWallet)
                Default
                Empty
                >>= flip
                    verify
                    [ expectField
                        (#balance . #reward . #toNatural)
                        (`shouldBe` 0)
                    , expectField
                        (#balance . #available . #toNatural)
                        (`shouldBe` 100_000_000)
                    , expectField
                        (#balance . #total . #toNatural)
                        (`shouldBe` 100_000_000)
                    ]
            let expectedSourceDistribution =
                    [(100_000_000, 1)]
            request @ApiUtxoStatistics
                ctx
                (Link.getUTxOsStatistics @'Shelley sourceWallet)
                Default
                Empty
                >>= flip
                    verify
                    [ expectField
                        #distribution
                        ( (`shouldBe` expectedSourceDistribution)
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
            let perEntryAdaQuantity = Coin 1_462_500
            let perEntryAssetCount = 1
            let batchSize = 20
            sourceAddresses <-
                take 20 . map (apiAddress . view #id)
                    <$> listAddresses @n ctx sourceWallet
            replicateM_ 6
                $ liftIO
                $ _mintSeaHorseAssets
                    ctx
                    perEntryAssetCount
                    batchSize
                    perEntryAdaQuantity
                    (CA.unsafeMkAddress . unAddress <$> sourceAddresses)
            waitForTxImmutability ctx

            -- Check that minting was successful, and that the balance and UTxO
            -- distribution have both changed accordingly:
            let expectedBalanceAda = 275_500_000
            let expectedAssetCount = 20
            request @ApiWallet
                ctx
                (Link.getWallet @'Shelley sourceWallet)
                Default
                Empty
                >>= flip
                    verify
                    [ expectField
                        (#balance . #available . #toNatural)
                        (`shouldBe` expectedBalanceAda)
                    , expectField
                        (#balance . #total . #toNatural)
                        (`shouldBe` expectedBalanceAda)
                    , expectField
                        (#assets . #available)
                        ((`shouldBe` expectedAssetCount) . length . toList)
                    ]
            let expectedSourceDistributionAfterMinting =
                    [ (10_000_000, 120)
                    , (100_000_000, 1)
                    ]
            request @ApiUtxoStatistics
                ctx
                (Link.getUTxOsStatistics @'Shelley sourceWallet)
                Default
                Empty
                >>= flip
                    verify
                    [ expectField
                        #distribution
                        ( (`shouldBe` expectedSourceDistributionAfterMinting)
                            . Map.toList
                            . Map.filter (> 0)
                        )
                    ]

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds =
                    targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)

            -- Create a migration plan, and check that the plan is only
            -- partially complete:
            let ep = Link.createMigrationPlan @'Shelley sourceWallet
            request @(ApiWalletMigrationPlan n)
                ctx
                ep
                Default
                (Json [json|{addresses: #{targetAddressIds}}|])
                >>= flip
                    verify
                    [ expectResponseCode HTTP.status202
                    , expectField
                        (#selections)
                        ((`shouldBe` 1) . length)
                    , expectField
                        id
                        ((`shouldBe` 102) . apiPlanTotalInputCount)
                    , expectField
                        id
                        ((`shouldBe` 1) . apiPlanTotalOutputCount)
                    , expectField
                        (#balanceSelected . #ada . #toNatural)
                        (`shouldBe` 247_712_500)
                    , expectField
                        (#balanceLeftover . #ada . #toNatural)
                        (`shouldBe` 27_787_500)
                    , expectField
                        (#balanceSelected . #assets)
                        ((.> 0) . length . toList)
                    , expectField
                        (#balanceLeftover . #assets)
                        ((.> 0) . length . toList)
                    ]

    describe
        "SHELLEY_MIGRATE_01 - \
        \After a migration operation successfully completes, the correct \
        \amounts eventually become available in the target wallet for an \
        \arbitrary number of specified addresses, and the balance of the \
        \source wallet is completely depleted."
        $ do
            testAddressCycling 1
            testAddressCycling 3
            testAddressCycling 10

    it
        "SHELLEY_MIGRATE_02 - \
        \Can migrate a large wallet requiring more than one transaction."
        $ \ctx -> runResourceT @IO $ do
            bigDustWallet <- liftIO $ bigDustWalletMnemonic (_faucet ctx)

            let bigDustUTxOSize = 200 :: Int
            -- Create a large source wallet from which funds will be migrated:
            sourceWallet <-
                unsafeResponse
                    <$> postWallet
                        ctx
                        ( Json
                            [json|{
                "name": "Big Shelley Wallet",
                "mnemonic_sentence": #{someMnemonicToWords bigDustWallet},
                "passphrase": #{fixturePassphrase},
                "address_pool_gap": #{bigDustUTxOSize}
            }|]
                        )

            let transactions :: IO String
                transactions = do
                    (_ , txs) <- unsafeRequest @[ApiTransaction n] ctx
                        (Link.listTransactions @'Shelley sourceWallet) Empty

                    pure $
                        "Source wallet balance is not correct. "
                        <> "Transactions in the wallet are: "
                        <> show (length txs) <> "\n"
                        <> TL.unpack (pShowNoColor txs)

            sourceBalance <- eventuallyReport transactions $ do
                response <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley sourceWallet)
                        Default
                        Empty
                verify
                    response
                    [ expectField
                        (#balance . #available . #toNatural)
                        (`shouldBe` 10_000_100_000_000)
                    ]
                return
                    $ getFromResponse
                        (#balance . #available . #toNatural)
                        response

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds =
                    targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)

            -- Compute the expected migration plan:
            responsePlan <-
                request @(ApiWalletMigrationPlan n)
                    ctx
                    (Link.createMigrationPlan @'Shelley sourceWallet)
                    Default
                    (Json [json|{addresses: #{targetAddressIds}}|])
            verify
                responsePlan
                [ expectResponseCode HTTP.status202
                , expectField
                    (#totalFee . #toNatural)
                    ( `shouldBe`
                        if _mainEra ctx >= ApiBabbage
                            then 3_120_200
                            else 3_119_800
                    )
                , expectField
                    (#selections)
                    ((`shouldBe` 2) . length)
                , expectField
                    (#balanceLeftover . #ada . #toNatural)
                    (`shouldBe` 0)
                , expectField
                    (#balanceSelected . #ada . #toNatural)
                    (`shouldBe` 10_000_100_000_000)
                ]
            let expectedFee =
                    getFromResponse
                        (#totalFee . #toNatural)
                        responsePlan
            let balanceLeftover =
                    getFromResponse
                        (#balanceLeftover . #ada . #toNatural)
                        responsePlan

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
            response <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley targetWallet)
                    Default
                    Empty
            verify
                response
                [ expectField
                    (#balance . #available . #toNatural)
                    (`shouldBe` expectedTargetBalance)
                , expectField
                    (#balance . #total . #toNatural)
                    (`shouldBe` expectedTargetBalance)
                ]

            -- Analyse the target wallet's UTxO distribution:
            responseStats <-
                request @ApiUtxoStatistics
                    ctx
                    (Link.getUTxOsStatistics @'Shelley targetWallet)
                    Default
                    Empty
            verify
                responseStats
                [ expectField
                    (#distribution)
                    ((`shouldBe` (Just 2)) . Map.lookup 10_000_000_000_000)
                ]

            -- Check that the source wallet has the expected leftover balance:
            responseFinalSourceBalance <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley sourceWallet)
                    Default
                    Empty
            verify
                responseFinalSourceBalance
                [ expectResponseCode HTTP.status200
                , expectField
                    (#balance . #available)
                    (`shouldBe` ApiAmount 0)
                , expectField
                    (#balance . #total)
                    (`shouldBe` ApiAmount 0)
                ]

    it
        "SHELLEY_MIGRATE_03 - \
        \Migrating an empty wallet should fail."
        $ \ctx -> runResourceT $ do
            sourceWallet <- emptyWallet ctx
            let sourceWalletId = sourceWallet ^. walletId
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds =
                    targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)
            let ep = Link.migrateWallet @'Shelley sourceWallet
            response <-
                request @[ApiTransaction n] ctx ep Default
                    $ Json
                        [json|
                        { passphrase: #{fixturePassphrase}
                        , addresses: #{targetAddressIds}
                        }|]
            verify
                response
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate sourceWalletId)
                ]

    Hspec.it
        "SHELLEY_MIGRATE_04 - \
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
            let targetAddressIds =
                    targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)

            -- Create a migration plan:
            let endpointPlan = (Link.createMigrationPlan @'Shelley sourceWallet)
            responsePlan <-
                request @(ApiWalletMigrationPlan n)
                    ctx
                    endpointPlan
                    Default
                    $ Json [json|{addresses: #{targetAddressIds}}|]
            -- Verify the fee is as expected:
            verify
                responsePlan
                [ expectResponseCode HTTP.status202
                , expectField #totalFee (`shouldBe` ApiAmount feeExpected)
                , expectField #selections ((`shouldBe` 1) . length)
                ]

            -- Perform a migration:
            let endpointMigrate = Link.migrateWallet @'Shelley sourceWallet
            responseMigrate <-
                request @[ApiTransaction n] ctx endpointMigrate Default
                    $ Json
                        [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: #{targetAddressIds}
                    }|]
            -- Verify the fee is as expected:
            verify
                responseMigrate
                [ expectResponseCode HTTP.status202
                , expectField id ((`shouldBe` 1) . length)
                , expectField id
                    $ (`shouldBe` feeExpected)
                        . fromIntegral
                        . sum
                        . fmap apiTransactionFee
                ]

    it
        "SHELLEY_MIGRATE_05 - \
        \Migration fails if the wrong passphrase is supplied."
        $ \ctx -> runResourceT $ do
            -- Restore a Shelley wallet with funds, to act as a source wallet:
            sourceWallet <- fixtureWallet ctx

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds =
                    targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)

            -- Attempt to perform a migration:
            response <-
                request @[ApiTransaction n]
                    ctx
                    (Link.migrateWallet @'Shelley sourceWallet)
                    Default
                    ( Json
                        [json|
                    { passphrase: "not-the-right-passphrase"
                    , addresses: #{targetAddressIds}
                    }|]
                    )
            verify
                response
                [ expectResponseCode HTTP.status403
                , expectErrorMessage errMsg403WrongPass
                ]

    it
        "SHELLEY_MIGRATE_06 - \
        \It's possible to migrate to any valid address."
        $ \ctx -> runResourceT $ do
            -- Create a Shelley address:
            wShelley <- emptyWallet ctx
            addrs <- listAddresses @n ctx wShelley
            let addrShelley = (addrs !! 1) ^. #id

            -- Create an Icarus address:
            addrIcarus <-
                encodeAddress (sNetworkId @n) . head . icarusAddresses @n
                    <$> Mnemonics.generateSome Mnemonics.M15

            -- Create a Byron address:
            addrByron <-
                encodeAddress (sNetworkId @n) . head . randomAddresses @n
                    <$> Mnemonics.generateSome Mnemonics.M12

            -- Create a source wallet:
            sourceWallet <- emptyWallet ctx

            -- Initiate a migration to all address types:
            response <-
                request @[ApiTransaction n]
                    ctx
                    (Link.migrateWallet @'Shelley sourceWallet)
                    Default
                    ( Json
                        [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: [#{addrShelley}, #{addrIcarus}, #{addrByron}]
                    }|]
                    )
            verify
                response
                [ expectResponseCode HTTP.status403
                , expectErrorMessage
                    (errMsg403NothingToMigrate (sourceWallet ^. walletId))
                ]

    it
        "SHELLEY_MIGRATE_07 - \
        \Including an invalidly-formatted passphrase results in a parser error."
        $ \ctx -> runResourceT $ do
            sourceWallet <- emptyWallet ctx
            response <-
                request @[ApiTransaction n]
                    ctx
                    (Link.migrateWallet @'Shelley sourceWallet)
                    Default
                    (NonJson "{passphrase:,}")
            verify
                response
                [ expectResponseCode HTTP.status400
                , expectErrorMessage
                    "Unexpected 'passphrase:,}', \
                    \expecting record key literal or }"
                ]

    Hspec.it
        "SHELLEY_MIGRATE_08 - \
        \It's possible to migrate a wallet with many small ada quantities, \
        \provided that the total balance is significantly greater than the \
        \minimum ada quantity for an output."
        $ \ctx -> runResourceT @IO $ do
            onlyDustWallet <- liftIO $ onlyDustWalletMnemonic (_faucet ctx)

            -- Create a source wallet with many small ada quantities:
            sourceWallet <-
                unsafeResponse
                    <$> postWallet
                        ctx
                        ( Json
                            [json|{
                    "name": "Shelley Wallet",
                    "mnemonic_sentence": #{someMnemonicToWords onlyDustWallet},
                    "passphrase": #{fixturePassphrase}
                }|]
                        )
            sourceBalance <- eventually "Source wallet balance is correct." $ do
                response <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley sourceWallet)
                        Default
                        Empty
                verify
                    response
                    [ expectField
                        (#balance . #available . #toNatural)
                        (`shouldBe` 43_000_000)
                    , expectField
                        (#balance . #total . #toNatural)
                        (`shouldBe` 43_000_000)
                    ]
                pure
                    $ getFromResponse
                        (#balance . #available . #toNatural)
                        response

            -- Analyse the source wallet's UTxO distribution:
            let expectedSourceDistribution =
                    [ (1_000_000, 3)
                    , (10_000_000, 6)
                    , (100_000_000, 1)
                    ]
            responseSourceDistribution <-
                request @ApiUtxoStatistics
                    ctx
                    (Link.getUTxOsStatistics @'Shelley sourceWallet)
                    Default
                    Empty
            verify
                responseSourceDistribution
                [ expectField
                    #distribution
                    ( (`shouldBe` expectedSourceDistribution)
                        . Map.toList
                        . Map.filter (> 0)
                    )
                ]

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds =
                    targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)

            -- Compute the expected migration plan:
            let feeExpected =
                    if _mainEra ctx >= ApiBabbage
                        then 254_700
                        else 254_500
            responsePlan <-
                request @(ApiWalletMigrationPlan n)
                    ctx
                    (Link.createMigrationPlan @'Shelley sourceWallet)
                    Default
                    (Json [json|{addresses: #{targetAddressIds}}|])
            verify
                responsePlan
                [ expectResponseCode HTTP.status202
                , expectField #totalFee (`shouldBe` ApiAmount feeExpected)
                , expectField #selections ((`shouldBe` 1) . length)
                ]

            -- Perform the migration:
            let ep = Link.migrateWallet @'Shelley sourceWallet
            responseMigrate <-
                request @[ApiTransaction n] ctx ep Default
                    $ Json
                        [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: #{targetAddressIds}
                    }|]

            -- Verify the fee is as expected:
            verify
                responseMigrate
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
            request @ApiWallet
                ctx
                (Link.getWallet @'Shelley targetWallet)
                Default
                Empty
                >>= flip
                    verify
                    [ expectField
                        (#balance . #available)
                        (`shouldBe` ApiAmount expectedBalance)
                    , expectField
                        (#balance . #total)
                        (`shouldBe` ApiAmount expectedBalance)
                    ]

            -- Analyse the target wallet's UTxO distribution:
            let expectedTargetDistribution = [(100_000_000, 1)]
            responseTargetDistribution <-
                request @ApiUtxoStatistics
                    ctx
                    (Link.getUTxOsStatistics @'Shelley targetWallet)
                    Default
                    Empty
            verify
                responseTargetDistribution
                [ expectField
                    #distribution
                    ( (`shouldBe` expectedTargetDistribution)
                        . Map.toList
                        . Map.filter (> 0)
                    )
                ]

    Hspec.it
        "SHELLEY_MIGRATE_09 - \
        \Can migrate a wallet that has rewards."
        $ \ctx -> runResourceT @IO $ do

            -- Create a source wallet with rewards:
            (sourceWallet, _sourceWalletMnemonic) <- rewardWallet ctx

            let utxoBalance = sourceWallet
                    ^. (#balance . #available . #toNatural)
            let rewardBalance0 = sourceWallet ^. -- may increase during test
                    (#balance . #reward . #toNatural)

            utxoBalance .> 99_000_000_000
            liftIO $ rewardBalance0 .> 0

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds =
                    targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)

            -- Perform a migration:
            let ep = Link.migrateWallet @'Shelley sourceWallet
            responseMigrate <-
                request @[ApiTransaction n] ctx ep Default
                    $ Json
                        [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: #{targetAddressIds}
                    }|]

            -- Verify the fee is as expected:
            let expectedFee =
                    if _mainEra ctx >= ApiBabbage
                        then 139_200
                        else 139_000
            verify
                responseMigrate
                [ expectResponseCode HTTP.status202
                , expectField id ((`shouldBe` 1) . length)
                , expectField id
                    $ (`shouldBe` expectedFee)
                        . sum
                        . fmap apiTransactionFee
                ]

            waitForTxImmutability ctx

            -- Check that funds become available in the target wallet:
            request @ApiWallet
                ctx
                (Link.getWallet @'Shelley targetWallet)
                Default
                Empty
                >>= flip
                    verify
                    [ expectField
                        (#balance . #available . #toNatural)
                        (.>= (rewardBalance0 + utxoBalance - expectedFee))
                    , expectField
                        (#balance . #total . #toNatural)
                        (.>= (rewardBalance0 + utxoBalance - expectedFee))
                    ]

            -- Check that the source wallet has been depleted:
            request @ApiWallet
                ctx
                (Link.getWallet @'Shelley sourceWallet)
                Default
                Empty
                >>= flip
                    verify
                    [ expectResponseCode HTTP.status200
                    , expectField
                        (#balance . #available)
                        (`shouldBe` ApiAmount 0)
                    , expectField
                        (#balance . #total)
                        (`shouldBe` ApiAmount 0)
                    ]

    Hspec.it
        "SHELLEY_MIGRATE_MULTI_ASSET_01 - \
        \Can migrate a multi-asset wallet."
        $ \ctx -> runResourceT @IO $ do
            -- Restore a source wallet with funds:
            sourceWallet <- fixtureMultiAssetWallet ctx

            -- Wait for the source wallet balance to be correct:
            let expectedAdaBalance = 40_000_000
            sourceBalance <- eventually "Source wallet balance is correct." $ do
                response <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley sourceWallet)
                        Default
                        Empty
                verify
                    response
                    [ expectField
                        (#balance . #available . #toNatural)
                        (`shouldBe` expectedAdaBalance)
                    , expectField
                        (#balance . #total . #toNatural)
                        (`shouldBe` expectedAdaBalance)
                    , expectField
                        (#assets . #available)
                        ((`shouldBe` 8) . length . toList)
                    , expectField
                        (#assets . #total)
                        ((`shouldBe` 8) . length . toList)
                    ]
                let balanceAda =
                        response
                            & getFromResponse (#balance . #available . #toNatural)
                            & fromIntegral
                            & Coin
                let balanceAssets =
                        response
                            & getFromResponse (#assets . #available)
                            & ApiWalletAssets.toTokenMap
                pure $ TokenBundle balanceAda balanceAssets

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds =
                    targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)

            -- Create a migration plan:
            let endpointPlan = (Link.createMigrationPlan @'Shelley sourceWallet)
            responsePlan <-
                request @(ApiWalletMigrationPlan n)
                    ctx
                    endpointPlan
                    Default
                    $ Json [json|{addresses: #{targetAddressIds}}|]

            -- Verify the plan is as expected:
            let expectedFee =
                    if _mainEra ctx >= ApiBabbage
                        then 191_000
                        else 190_800
            verify
                responsePlan
                [ expectResponseCode HTTP.status202
                , expectField
                    (#totalFee . #toNatural)
                    (`shouldBe` expectedFee)
                , expectField
                    (#selections)
                    ((`shouldBe` 1) . length)
                , expectField
                    id
                    ((`shouldBe` 3) . apiPlanTotalInputCount)
                , expectField
                    id
                    ((`shouldBe` 1) . apiPlanTotalOutputCount)
                , expectField
                    (#balanceSelected . #ada)
                    (`shouldBe` ApiAmount.fromCoin (view #coin sourceBalance))
                , expectField
                    (#balanceLeftover . #ada . #toNatural)
                    (`shouldBe` 0)
                , expectField
                    (#balanceSelected . #assets)
                    ( `shouldBe`
                        ApiWalletAssets.fromTokenMap
                            (view #tokens sourceBalance)
                    )
                , expectField
                    (#balanceLeftover . #assets)
                    (`shouldBe` mempty)
                ]

            -- Perform a migration:
            let endpointMigrate = Link.migrateWallet @'Shelley sourceWallet
            responseMigrate <-
                request @[ApiTransaction n] ctx endpointMigrate Default
                    $ Json
                        [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: #{targetAddressIds}
                    }|]

            -- Verify the fee is as expected:
            verify
                responseMigrate
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
            let expectedSourceBalance =
                    ApiWalletAssets.fromTokenMap
                        (view #tokens sourceBalance)
            request @ApiWallet
                ctx
                (Link.getWallet @'Shelley targetWallet)
                Default
                Empty
                >>= flip
                    verify
                    [ expectField
                        (#balance . #available)
                        (`shouldBe` ApiAmount expectedTargetBalance)
                    , expectField
                        (#balance . #total)
                        (`shouldBe` ApiAmount expectedTargetBalance)
                    , expectField
                        (#assets . #available)
                        (`shouldBe` expectedSourceBalance)
                    , expectField
                        (#assets . #total)
                        (`shouldBe` expectedSourceBalance)
                    ]

            -- Check that the source wallet has been depleted:
            responseFinalSourceBalance <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley sourceWallet)
                    Default
                    Empty
            verify
                responseFinalSourceBalance
                [ expectResponseCode HTTP.status200
                , expectField
                    (#balance . #available)
                    (`shouldBe` ApiAmount 0)
                , expectField
                    (#balance . #total)
                    (`shouldBe` ApiAmount 0)
                , expectField
                    (#assets . #available)
                    (`shouldBe` mempty)
                , expectField
                    (#assets . #total)
                    (`shouldBe` mempty)
                ]
  where
    -- Compute the fee associated with an API transaction.
    apiTransactionFee :: ApiTransaction n -> Natural
    apiTransactionFee = view (#fee . #toNatural)

    migrateWallet
        :: Context
        -> ApiWallet
        -> [ApiAddress n]
        -> IO ()
    migrateWallet ctx src targets = do
        (status, _) <-
            request @(ApiWalletMigrationPlan n)
                ctx
                endpointCreateMigrationPlan
                Default
                payloadCreateMigrationPlan
        when (status == HTTP.status202) $ do
            -- The above request returns '403 Nothing to Migrate' when done.

            -- 1. Forget all pending transactions to unlock any locked UTxO:
            (_, txs) <-
                unsafeRequest
                    @[ApiTransaction n]
                    ctx
                    endpointListTxs
                    Empty
            forM_ txs $ forgetTxIf ((== ApiT Pending) . view #status)

            -- 2. Attempt to migrate:
            _ <-
                request @[ApiTransaction n]
                    ctx
                    endpointMigrateWallet
                    Default
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
        payloadMigrateWallet =
            Json
                [json|
            { "passphrase": #{fixturePassphrase}
            , "addresses": #{targets}
            }|]

        forgetTxIf predicate tx
            | predicate tx =
                void $ unsafeRequest @() ctx (endpointForget tx) Empty
            | otherwise =
                pure ()

    testAddressCycling targetAddressCount = do
        let title =
                mconcat
                    [ "Migration from Shelley wallet to target address count: "
                    , show targetAddressCount
                    , "."
                    ]
        it title $ \ctx -> runResourceT $ do
            -- Restore a Shelley wallet with funds, to act as a source wallet:
            sourceWallet <- fixtureWallet ctx
            let sourceBalance =
                    view (#balance . #available . #toNatural) sourceWallet

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            targetAddresses <- listAddresses @n ctx targetWallet
            let targetAddressIds =
                    take targetAddressCount targetAddresses
                        <&> (\(ApiTypes.ApiAddressWithPath addrId _ _) -> addrId)

            -- Create a migration plan:
            response0 <-
                request @(ApiWalletMigrationPlan n)
                    ctx
                    (Link.createMigrationPlan @'Shelley sourceWallet)
                    Default
                    (Json [json|{addresses: #{targetAddressIds}}|])
            verify
                response0
                [ expectResponseCode HTTP.status202
                , expectField #totalFee (.> ApiAmount 0)
                ]
            let expectedFee =
                    getFromResponse (#totalFee . #toNatural) response0

            -- Perform a migration from the source wallet to the target wallet:
            response1 <-
                request @[ApiTransaction n]
                    ctx
                    (Link.migrateWallet @'Shelley sourceWallet)
                    Default
                    ( Json
                        [json|
                    { passphrase: #{fixturePassphrase}
                    , addresses: #{targetAddressIds}
                    }|]
                    )
            verify
                response1
                [ expectResponseCode HTTP.status202
                , expectField id (`shouldSatisfy` (not . null))
                ]

            waitForTxImmutability ctx

            -- Check that funds have become available in the target wallet:
            let expectedTargetBalance = sourceBalance - expectedFee
            response2 <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley targetWallet)
                    Default
                    Empty
            verify
                response2
                [ expectField
                    (#balance . #available)
                    (`shouldBe` ApiAmount expectedTargetBalance)
                , expectField
                    (#balance . #total)
                    (`shouldBe` ApiAmount expectedTargetBalance)
                ]

            -- Check that the source wallet has a balance of zero:
            responseFinalSourceBalance <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley sourceWallet)
                    Default
                    Empty
            verify
                responseFinalSourceBalance
                [ expectField
                    (#balance . #available)
                    (`shouldBe` ApiAmount 0)
                , expectField
                    (#balance . #total)
                    (`shouldBe` ApiAmount 0)
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
