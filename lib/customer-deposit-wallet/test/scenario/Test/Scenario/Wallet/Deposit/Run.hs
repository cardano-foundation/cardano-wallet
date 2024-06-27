{-|
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Execute usage scenarios for the deposit wallet.
-}
module Test.Scenario.Wallet.Deposit.Run
    ( main
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub
    , generate
    , toXPub
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
import qualified Data.ByteString.Char8 as B8
import qualified Test.Scenario.Wallet.Deposit.Exchanges as Exchanges

main :: IO ()
main =
    hspecMain
    $ aroundAll withScenarioEnvMock scenarios

scenarios :: SpecWith ScenarioEnv
scenarios = do
    describe "Scenarios for centralized exchanges" $ do
        it "Restore a wallet" $ \env ->
            withWalletEnvMock env $
                Exchanges.scenarioRestore xpub

        it "Start a wallet" $ \env ->
            withWalletEnvMock env $ \w -> do
                Exchanges.scenarioRestore xpub w
                Exchanges.scenarioStart w

        it "Assign an address to a customer ID" $ \env -> do
            withWalletEnvMock env $ \walletEnv ->
                Wallet.withWalletInit walletEnv xpub 1
                    Exchanges.scenarioCreateAddressList

    describe "Temporary tests" $ do
        it "Wallet receives funds that are sent to customer address" $ \env -> do
            withWalletEnvMock env $ \walletEnv ->
                Wallet.withWalletInit walletEnv xpub 1 $
                    testBalance env

xpub :: XPub
xpub =
    toXPub
    $ generate (B8.pack "random seed for a testing xpub lala") B8.empty

testBalance
    :: ScenarioEnv -> Wallet.WalletInstance -> IO ()
testBalance env w = do
    address <- Wallet.createAddress customer w
    payFromFaucet env [(address, coin)]
    value <- Wallet.availableBalance w
    assert $ coin == value
  where
    customer = 7
    coin = ada 12
