-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Execute usage scenarios for the deposit wallet.
module Test.Scenario.Wallet.Deposit.Run
    ( main
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv
    , XPub
    , generate
    , toXPub
    )
import Cardano.Wallet.Deposit.Pure.State.Creation
    ( Credentials (..)
    )
import Control.Tracer
    ( nullTracer
    )
import Test.Hspec
    ( SpecWith
    , describe
    , it
    )
import Test.Hspec.Extra
    ( aroundAll
    , hspecMain
    )
import Test.Scenario.Blockchain
    ( ScenarioEnv
    , ada
    , assert
    , payFromFaucet
    , withScenarioEnvMock
    , withWalletEnvMock
    )

import qualified Cardano.Wallet.Deposit.IO as Wallet
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as SBS
import qualified Test.Scenario.Wallet.Deposit.Exchanges as Exchanges

main :: IO ()
main =
    hspecMain
        $ aroundAll withScenarioEnvMock scenarios

scenarios :: SpecWith ScenarioEnv
scenarios = do
    describe "Scenarios for centralized exchanges" $ do
        it "0. Restore a wallet" $ \env ->
            withWalletEnvMock env
                $ Exchanges.scenarioRestore xpub

        it "0. Start a wallet" $ \env ->
            withWalletEnvMock env $ \w -> do
                Exchanges.scenarioRestore xpub w
                Exchanges.scenarioStart w

        it "1. Assign an address to a customer ID" $ \env -> do
            withWalletEnvMock env $ \walletEnv ->
                Wallet.withWalletInit
                    nullTracer
                    walletEnv
                    (XPubCredentials $ freshXPub 1)
                    32
                    Exchanges.scenarioCreateAddressList

        it "4. Create payments to a different wallet" $ \env -> do
            withWalletEnvMock env $ \walletEnv ->
                Wallet.withWalletInit nullTracer
                    walletEnv (XPubCredentials xpub) 32
                    $ Exchanges.scenarioCreatePayment xprv env mockAddress

    describe "Temporary tests" $ do
        it "Wallet receives funds that are sent to customer address" $ \env -> do
            withWalletEnvMock env $ \walletEnv ->
                Wallet.withWalletInit
                    nullTracer
                    walletEnv
                    (XPubCredentials $ freshXPub 0)
                    8
                    $ testBalance env

xpub :: XPub
xpub = toXPub xprv

xprv :: XPrv
xprv = generate (B8.pack "random seed for a testing xpub lala") B8.empty

freshXPub :: Integer -> XPub
freshXPub i =
    toXPub
        $ generate
            (B8.pack $ "random seed for a testing xpub lala" <> show i)
            B8.empty

mockAddress :: Read.Address
mockAddress =
    Read.mkEnterpriseAddress
        Read.MainnetTag
        (SBS.pack $ replicate 32 0)

testBalance
    :: ScenarioEnv -> Wallet.WalletInstance -> IO ()
testBalance env w = do
    Just address <- Wallet.customerAddress customer w
    payFromFaucet env [(address, coin)]
    value <- Wallet.availableBalance w
    assert $ coin == value
  where
    customer = 7
    coin = ada 12
