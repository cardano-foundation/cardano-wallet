{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Byron.Scenario.API.Migrations
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiTransaction
    , ApiWalletMigrationInfo
    , DecodeAddress (..)
    , DecodeStakeAddress (..)
    , EncodeAddress (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..), PaymentAddress (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe, shouldSatisfy )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , emptyRandomWalletMws
    , eventually
    , expectErrorMessage
    , expectField
    , expectResponseCode
    , fixtureIcarusWallet
    , fixturePassphrase
    , fixtureRandomWallet
    , getFromResponse
    , json
    , randomAddresses
    , request
    , verify
    , (.>)
    )
import Test.Integration.Framework.TestData
    ( errMsg400ParseError )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec
    :: forall (n :: NetworkDiscriminant) t.
        ( PaymentAddress n IcarusKey
        , PaymentAddress n ByronKey
        , EncodeAddress n
        , DecodeAddress n
        , DecodeStakeAddress n
        )
    => SpecWith (Context t)
spec = do

    describe "BYRON_MIGRATE" $ do
        scenario_MIGRATE_01 @n fixtureRandomWallet
        scenario_MIGRATE_02 @n fixtureRandomWallet 1
        scenario_MIGRATE_02 @n fixtureRandomWallet 3
        scenario_MIGRATE_02 @n fixtureRandomWallet 10
        scenario_MIGRATE_02 @n fixtureIcarusWallet 1
        scenario_MIGRATE_02 @n fixtureIcarusWallet 3
        scenario_MIGRATE_02 @n fixtureIcarusWallet 10

--
-- Scenarios
--
scenario_MIGRATE_01
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        )
    => (Context t -> IO ApiByronWallet)
    -> SpecWith (Context t)
scenario_MIGRATE_01 fixtureSource = it title $ \ctx -> do
    wSrc <- fixtureSource ctx

    r <- request @[ApiTransaction n] ctx
         (Link.migrateWallet @'Byron wSrc)
         Default
         (NonJson "{passphrase:,}")
    expectResponseCode @IO HTTP.status400 r
    expectErrorMessage errMsg400ParseError r
  where
    title = "BYRON_MIGRATE_01 - invalid payload, parser error"

scenario_MIGRATE_02
    :: forall (n :: NetworkDiscriminant) t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        , PaymentAddress n ByronKey
        )
    => (Context t -> IO ApiByronWallet)
    -> Int
    -> SpecWith (Context t)
scenario_MIGRATE_02 fixtureSource addrCount = it title $ \ctx -> do
    -- Restore a Byron wallet with funds, to act as a source wallet:
    wSrc <- fixtureSource ctx
    let originalBalance =
            view (#balance . #available . #getQuantity) wSrc

    -- Create an empty target wallet:
    (wDest, mw) <- emptyRandomWalletMws ctx
    let addresses :: [Text] =
            take addrCount $ encodeAddress @n <$> randomAddresses @n mw

    -- Calculate the expected migration fee:
    r0 <- request @ApiWalletMigrationInfo ctx
          (Link.getMigrationInfo @'Byron wSrc) Default Empty
    verify r0
        [ expectResponseCode @IO HTTP.status200
        , expectField #migrationCost (.> Quantity 0)
        ]
    let expectedFee = getFromResponse (#migrationCost . #getQuantity) r0

    -- Perform a migration from the source wallet to the target wallet:
    r1 <- request @[ApiTransaction n] ctx
          (Link.migrateWallet @'Byron wSrc)
          Default
          (Json [json|
              { passphrase: #{fixturePassphrase}
              , addresses: #{addresses}
              }|])
    verify r1
        [ expectResponseCode @IO HTTP.status202
        , expectField id (`shouldSatisfy` (not . null))
        ]

    -- Check that funds become available in the target wallet:
    let expectedBalance = originalBalance - expectedFee
    eventually "Wallet has expectedBalance" $ do
        r2 <- request @ApiByronWallet ctx
              (Link.getWallet @'Byron wDest) Default Empty
        verify r2
            [ expectField
                (#balance . #available)
                (`shouldBe` Quantity expectedBalance)
            , expectField
                (#balance . #total)
                (`shouldBe` Quantity expectedBalance)
            ]
  where
    title = "BYRON_MIGRATE_02 - after a migration operation successfully \
            \completes, the correct amount eventually becomes available \
            \in the target wallet for an arbitrary number of specified addresses."
