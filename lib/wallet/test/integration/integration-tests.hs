{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Main where

import Prelude

import Cardano.Wallet.Launch.Cluster
    ( FileOf (..)
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( ignoreInBabbage
    , ignoreInConway
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (..)
    )
import Cardano.Wallet.Test.Integration.Setup
    ( TestingCtx (..)
    , reportMMetrics
    , withContext
    , withTestsSetup
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Typeable
    ( Proxy (..)
    )
import GHC.TypeNats
    ( natVal
    )
import System.Environment
    ( lookupEnv
    )
import Test.Hspec.Core.Spec
    ( SpecM
    , describe
    , it
    , parallel
    , sequential
    )
import Test.Hspec.Extra
    ( aroundAll
    , hspecMain
    )
import UnliftIO.MVar
    ( newMVar
    )

import qualified Cardano.Wallet.Launch.Cluster as Cluster
import qualified Test.Integration.Scenario.API.Blocks as Blocks
import qualified Test.Integration.Scenario.API.Byron.Addresses as ByronAddresses
import qualified Test.Integration.Scenario.API.Byron.CoinSelections as ByronCoinSelections
import qualified Test.Integration.Scenario.API.Byron.HWWallets as ByronHWWallets
import qualified Test.Integration.Scenario.API.Byron.Migrations as ByronMigrations
import qualified Test.Integration.Scenario.API.Byron.Transactions as ByronTransactions
import qualified Test.Integration.Scenario.API.Byron.Wallets as ByronWallets
import qualified Test.Integration.Scenario.API.Network as Network
import qualified Test.Integration.Scenario.API.Shared.Addresses as SharedAddresses
import qualified Test.Integration.Scenario.API.Shared.Transactions as SharedTransactions
import qualified Test.Integration.Scenario.API.Shared.Wallets as SharedWallets
import qualified Test.Integration.Scenario.API.Shelley.Addresses as Addresses
import qualified Test.Integration.Scenario.API.Shelley.CoinSelections as CoinSelections
import qualified Test.Integration.Scenario.API.Shelley.HWWallets as HWWallets
import qualified Test.Integration.Scenario.API.Shelley.Migrations as Migrations
import qualified Test.Integration.Scenario.API.Shelley.Network as Network_
import qualified Test.Integration.Scenario.API.Shelley.Settings as Settings
import qualified Test.Integration.Scenario.API.Shelley.StakePools as StakePools
import qualified Test.Integration.Scenario.API.Shelley.Transactions as Transactions
import qualified Test.Integration.Scenario.API.Shelley.TransactionsNew as TransactionsNew
import qualified Test.Integration.Scenario.API.Shelley.Wallets as Wallets
import qualified Test.Integration.Scenario.CLI.Miscellaneous as MiscellaneousCLI
import qualified Test.Integration.Scenario.CLI.Network as NetworkCLI
import qualified Test.Integration.Scenario.CLI.Port as PortCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Addresses as AddressesCLI
import qualified Test.Integration.Scenario.CLI.Shelley.HWWallets as HWWalletsCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Transactions as TransactionsCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Wallets as WalletsCLI

main :: forall netId n. (netId ~ 42, n ~ 'Testnet netId) => IO ()
main = withTestsSetup $ \testDir (tr, tracers) -> do
    localClusterEra <- Cluster.clusterEraFromEnv
    let _noConway, _noBabbage :: SpecM a () -> SpecM a ()
        _noConway = ignoreInConway localClusterEra
        _noBabbage = ignoreInBabbage localClusterEra
        testnetMagic = Cluster.TestnetMagic (natVal (Proxy @netId))
    testDataDir <-
        FileOf . fromMaybe "."
            <$> lookupEnv "CARDANO_WALLET_TEST_DATA"
    metrics <- newMVar mempty
    let testingCtx = TestingCtx{..}
    hspecMain $ do
        describe "No backend required"
            $ parallel
            $ describe "Miscellaneous CLI tests" MiscellaneousCLI.spec

        aroundAll (withContext testingCtx) $ do
            describe "API Specifications" $ do
                parallel $ do
                    Addresses.spec @n
                    CoinSelections.spec @n
                    Blocks.spec
                    ByronAddresses.spec @n
                    ByronCoinSelections.spec @n
                    Wallets.spec @n
                    SharedWallets.spec @n
                    SharedAddresses.spec @n
                    SharedTransactions.spec @n
                    ByronWallets.spec @n
                    HWWallets.spec @n
                    Migrations.spec @n
                    ByronMigrations.spec @n
                    Transactions.spec @n
                    TransactionsNew.spec @n
                    Network.spec
                    Network_.spec
                    StakePools.spec @n
                    ByronTransactions.spec @n
                    ByronHWWallets.spec @n

            -- Possible conflict with StakePools - mark as not parallizable
            sequential Settings.spec

            parallel $ describe "CLI Specifications" $ do
                AddressesCLI.spec @n
                TransactionsCLI.spec @n
                WalletsCLI.spec @n
                HWWalletsCLI.spec @n
                PortCLI.spec
                NetworkCLI.spec
        describe "report metrics" $ do
            it "TTT" $ reportMMetrics metrics
