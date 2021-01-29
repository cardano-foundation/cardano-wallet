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
    ( entropyToMnemonic, genEntropy )
import Cardano.Wallet.Api.Types
    ( ApiT (..)
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
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import Test.Hspec
    ( SpecWith, describe, pendingWith )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldSatisfy )
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
    , fixturePassphrase
    , fixtureWallet
    , getFromResponse
    , icarusAddresses
    , json
    , listAddresses
    , postWallet
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
spec = describe "SHELLEY_MIGRATIONS" $ do
    it "SHELLEY_CALCULATE_01 - \
        \for non-empty wallet calculated fee is > zero."
        $ \ctx -> runResourceT $ do
            liftIO $ pendingWith "Migration endpoints temporarily disabled."
            w <- fixtureWallet ctx
            let ep = Link.getMigrationInfo @'Shelley w
            r <- request @ApiWalletMigrationInfo ctx ep Default Empty
            verify r
                [ expectResponseCode HTTP.status200
                , expectField (#migrationCost . #getQuantity)
                    (.> 0)
                ]

    it "SHELLEY_CALCULATE_02 - \
        \Cannot calculate fee for empty wallet."
        $ \ctx -> runResourceT $ do
            liftIO $ pendingWith "Migration endpoints temporarily disabled."
            w <- emptyWallet ctx
            let ep = Link.getMigrationInfo @'Shelley w
            r <- request @ApiWalletMigrationInfo ctx ep Default Empty
            verify r
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate $ w ^. walletId)
                ]

    describe "SHELLEY_CALCULATE_03 - \
        \Cannot estimate migration for Byron wallet using Shelley endpoint" $ do
          forM_ [ ("Byron", emptyRandomWallet)
                , ("Icarus", emptyIcarusWallet)
                ] $ \(walType, byronWallet) -> do

                it ("Cannot calculate Shelley migration using wallet: " ++ walType)
                    $ \ctx -> runResourceT $ do
                    liftIO $ pendingWith "Migration endpoints temporarily disabled."
                    w <- byronWallet ctx
                    let ep = Link.getMigrationInfo @'Shelley w
                    r <- request @ApiWalletMigrationInfo ctx ep Default Empty
                    expectResponseCode HTTP.status404 r
                    expectErrorMessage (errMsg404NoWallet $ w ^. walletId) r

    describe "SHELLEY_MIGRATE_01 - \
        \after a migration operation successfully completes, the correct \
        \amount eventually becomes available in the target wallet for arbitrary \
        \ number of specified addresses. Balance of source wallet = 0."
        $ do
              testAddressCycling 1
              testAddressCycling 3
              testAddressCycling 10

    Hspec.it "SHELLEY_MIGRATE_01_big_wallet - \
        \ migrate a big wallet requiring more than one tx" $ \ctx -> runResourceT @IO $ do
        liftIO $ pendingWith "Migration endpoints temporarily disabled."

        -- NOTE
        -- Special mnemonic for which 200 shelley funds are attached to in the
        -- genesis file.
        --
        -- Out of these 200 coins, 100 of them are of 1 ADA and are
        -- expected to be treated as dust. The rest are all worth:
        -- 10,000,000,000 lovelace.
        let mnemonics =
                ["radar", "scare", "sense", "winner", "little"
                , "jeans", "blue", "spell", "mystery", "sketch"
                , "omit", "time", "tiger", "leave", "load"] :: [Text]
        let payloadRestore = Json [json| {
                "name": "Big Shelley Wallet",
                "mnemonic_sentence": #{mnemonics},
                "passphrase": #{fixturePassphrase}
                } |]
        wOld <- unsafeResponse <$> postWallet ctx payloadRestore
        originalBalance <- eventually "wallet balance greater than 0" $ do
            r <- request @ApiWallet ctx
                (Link.getWallet @'Shelley wOld)
                Default
                Empty
            verify r
                [ expectField (#balance . #available) (.> Quantity 0)
                ]
            return $ getFromResponse
                (#balance . #available . #getQuantity) r

        -- Calculate the expected migration fee:
        rFee <- request @ApiWalletMigrationInfo ctx
            (Link.getMigrationInfo @'Shelley wOld)
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


        -- NOTE
        -- The migration typically involves many transactions being sent one by
        -- one. It may happen that one of these transaction is rolled back and
        -- simply discarded entirely from mem pools. There's no retry mechanism
        -- from the wallet _yet_, which means that such transactions must be
        -- manually retried by clients.
        --
        -- This 'migrateWallet' function does exactly this, and will try to make
        -- sure that rolledback functions are canceled and retried up until the
        -- full migration is done.
        liftIO $ migrateWallet ctx wOld [addr1]

        -- Check that funds become available in the target wallet: Because
        -- there's a bit of non-determinism in how the migration is really done,
        -- we can expect the final balance with exactitude. Yet, we still expect
        -- it to be not too far away from an ideal value.
        let expectedMinBalance = originalBalance - 2 * expectedFee - leftovers
        eventually "wallet balance ~ expectedBalance" $ do
            request @ApiWallet ctx
                (Link.getWallet @'Shelley wNew)
                Default
                Empty >>= flip verify
                [ expectField
                        (#balance . #available)
                        (.> (Quantity expectedMinBalance))
                , expectField
                        (#balance . #total)
                        (.> (Quantity expectedMinBalance))
                ]

        -- Analyze the target wallet UTxO distribution
        request @ApiUtxoStatistics ctx (Link.getUTxOsStatistics @'Shelley wNew)
            Default
            Empty >>= flip verify
            [ expectField
                #distribution
                ((`shouldBe` (Just 100)) . Map.lookup 100_000_000_000)
            ]

    it "SHELLEY_MIGRATE_02 - \
        \migrating an empty wallet should fail."
        $ \ctx -> runResourceT $ do
            liftIO $ pendingWith "Migration endpoints temporarily disabled."
            sourceWallet <- emptyWallet ctx
            targetWallet <- emptyWallet ctx
            addrs <- listAddresses @n ctx targetWallet
            let addr1 = (addrs !! 1) ^. #id
            let payload =
                    Json [json|
                        { passphrase: #{fixturePassphrase}
                        , addresses: [#{addr1}]
                        }|]
            let ep = Link.migrateWallet @'Shelley sourceWallet
            r <- request @[ApiTransaction n] ctx ep Default payload
            let srcId = sourceWallet ^. walletId
            verify r
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403NothingToMigrate srcId)
                ]

    Hspec.it "SHELLEY_MIGRATE_02 - \
        \migrating wallet with 'dust' (that complies with minUTxOValue) should pass."
        $ \ctx -> runResourceT @IO $ do
            liftIO $ pendingWith "Migration endpoints temporarily disabled."
            -- NOTE
            -- Special mnemonic for which wallet has dust
            -- (10 utxo with 43 ADA)
            let mnemonics =
                    ["either", "flip", "maple", "shift", "dismiss", "bridge"
                    , "sweet", "reveal", "green", "tornado", "need", "patient"
                    , "wall", "stamp", "pass"] :: [Text]
            let payloadRestore = Json [json| {
                    "name": "Dust Shelley Wallet",
                    "mnemonic_sentence": #{mnemonics},
                    "passphrase": #{fixturePassphrase}
                    } |]
            sourceWallet <- unsafeResponse <$> postWallet ctx payloadRestore
            originalBalance <- eventually "wallet balance greater than 0" $ do
                rg <- request @ApiWallet ctx
                    (Link.getWallet @'Shelley sourceWallet)
                    Default
                    Empty
                verify rg
                    [ expectField (#balance . #available) (.> Quantity 0)
                    ]
                pure $ getFromResponse (#balance. #available . #getQuantity)
                                 rg

            -- Calculate the expected migration fee:
            r0 <- request @ApiWalletMigrationInfo ctx
                (Link.getMigrationInfo @'Shelley sourceWallet) Default Empty
            verify r0
                [ expectResponseCode HTTP.status200
                , expectField #migrationCost (.> Quantity 0)
                ]
            let expectedFee = getFromResponse (#migrationCost . #getQuantity) r0

            targetWallet <- emptyWallet ctx
            addrs <- listAddresses @n ctx targetWallet
            let addr1 = (addrs !! 1) ^. #id
            let payload =
                    Json [json|
                        { passphrase: #{fixturePassphrase}
                        , addresses: [#{addr1}]
                        }|]
            let ep = Link.migrateWallet @'Shelley sourceWallet
            r <- request @[ApiTransaction n] ctx ep Default payload
            verify r
                [ expectResponseCode HTTP.status202 ]

            -- Check that funds become available in the target wallet:
            let expectedBalance = originalBalance - expectedFee
            eventually "targetWallet balance = expectedBalance" $ do
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

    it "SHELLEY_MIGRATE_03 - \
        \actual fee for migration is the same as the predicted fee."
        $ \ctx -> runResourceT $ do
            liftIO $ pendingWith "Migration endpoints temporarily disabled."
            -- Restore a Shelley wallet with funds.
            sourceWallet <- fixtureWallet ctx

            -- Request a migration fee prediction.
            let ep0 = (Link.getMigrationInfo @'Shelley sourceWallet)
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
            let ep1 = Link.migrateWallet @'Shelley sourceWallet
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

    it "SHELLEY_MIGRATE_04 - migration fails with a wrong passphrase" $ \ctx -> runResourceT $ do
        liftIO $ pendingWith "Migration endpoints temporarily disabled."
        -- Restore a Shelley wallet with funds, to act as a source wallet:
        sourceWallet <- fixtureWallet ctx

        -- Perform a migration from the source wallet to a target wallet:
        targetWallet <- emptyWallet ctx
        addrs <- listAddresses @n ctx targetWallet
        let addr1 = (addrs !! 1) ^. #id
        r0 <- request @[ApiTransaction n] ctx
            (Link.migrateWallet @'Shelley sourceWallet)
            Default
            (Json [json|
                { passphrase: "not-the-right-passphrase"
                , addresses: [#{addr1}]
                }|])
        verify r0
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]


    it "SHELLEY_MIGRATE_05 - I could migrate to any valid address" $ \ctx -> runResourceT $ do
      liftIO $ pendingWith "Migration endpoints temporarily disabled."
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

      sWallet <- emptyWallet ctx
      r <- request @[ApiTransaction n] ctx
          (Link.migrateWallet @'Shelley sWallet)
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

    it "SHELLEY_MIGRATE_07 - invalid payload, parser error" $ \ctx -> runResourceT $ do
      liftIO $ pendingWith "Migration endpoints temporarily disabled."
      sourceWallet <- emptyWallet ctx
      r <- request @[ApiTransaction n] ctx
          (Link.migrateWallet @'Shelley sourceWallet)
          Default
          (NonJson "{passphrase:,}")
      expectResponseCode HTTP.status400 r
      expectErrorMessage errMsg400ParseError r
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

    migrateWallet
        :: Context
        -> ApiWallet
        -> [(ApiT Address, Proxy n)]
        -> IO ()
    migrateWallet ctx src targets = do
        (st, _) <- request @ApiWalletMigrationInfo ctx endpointInfo Default Empty
        when (st == HTTP.status200) $ do -- returns '403 Nothing to Migrate' when done
            -- 1/ Forget all pending transactions to unlock any locked UTxO
            (_, txs) <- unsafeRequest @[ApiTransaction n] ctx endpointListTxs Empty
            forM_ txs $ forgetTxIf ((== ApiT Pending) . view #status)

            -- 2/ Attempt to migrate
            _ <- request @[ApiTransaction n] ctx endpointMigration Default payload

            -- 3/ Wait "long-enough" for transactions to have been inserted.
            waitForTxImmutability ctx

            -- 4/ Recurse, until the server tells us there's nothing left to migrate
            migrateWallet ctx src targets
      where
        endpointInfo =
            Link.getMigrationInfo @'Shelley src
        endpointMigration =
            Link.migrateWallet @'Shelley src
        endpointListTxs =
            Link.listTransactions @'Shelley src
        endpointForget =
            Link.deleteTransaction @'Shelley src

        payload = Json
            [json|{"passphrase": #{fixturePassphrase}, "addresses": #{targets}}|]

        forgetTxIf predicate tx
            | predicate tx =
                void $ unsafeRequest @() ctx (endpointForget tx) Empty
            | otherwise =
                pure ()

    testAddressCycling addrNum =
        it ("Migration from Shelley wallet to " ++ show addrNum ++ " addresses")
            $ \ctx -> runResourceT $ do
            liftIO $ pendingWith "Migration endpoints temporarily disabled."
            -- Restore a Shelley wallet with funds, to act as a source wallet:
            sourceWallet <- fixtureWallet ctx
            let originalBalance =
                        view (#balance. #available . #getQuantity)
                             sourceWallet

            -- Create an empty target wallet:
            targetWallet <- emptyWallet ctx
            addrs <- listAddresses @n ctx targetWallet
            let addrIds =
                    map (\(ApiTypes.ApiAddress theid _) -> theid) $
                    take addrNum addrs

            -- Calculate the expected migration fee:
            r0 <- request @ApiWalletMigrationInfo ctx
                (Link.getMigrationInfo @'Shelley sourceWallet) Default Empty
            verify r0
                [ expectResponseCode HTTP.status200
                , expectField #migrationCost (.> Quantity 0)
                ]
            let expectedFee = getFromResponse (#migrationCost . #getQuantity) r0

            -- Perform a migration from the source wallet to the target wallet:
            r1 <- request @[ApiTransaction n] ctx
                (Link.migrateWallet @'Shelley sourceWallet)
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
            let expectedBalance = originalBalance - expectedFee
            eventually "Wallet has expectedBalance" $ do
                r2 <- request @ApiWallet ctx
                    (Link.getWallet @'Shelley targetWallet) Default Empty
                verify r2
                    [ expectField
                            (#balance . #available)
                            (`shouldBe` Quantity expectedBalance)
                    , expectField
                            (#balance . #total)
                            (`shouldBe` Quantity expectedBalance)
                    ]

            -- Verify sourceWallet has balance 0
            r3 <- request @ApiWallet ctx
                (Link.getWallet @'Shelley sourceWallet) Default Empty
            verify r3
                [ expectField
                        (#balance . #available)
                        (`shouldBe` Quantity 0)
                , expectField
                        (#balance . #total)
                        (`shouldBe` Quantity 0)
                ]
