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

main :: IO ()
main =
    hspecMain
    $ aroundAll withScenarioEnvMock scenarios

scenarios :: SpecWith ScenarioEnv
scenarios = do
    describe "Temporary tests" $ do
        it "Wallet receives funds that are sent to customer address" $ \env -> do
            withWalletEnvMock env $ \walletEnv ->
                Wallet.withWalletInit xpub 1 walletEnv $
                    testBalance env

xpub :: XPub
xpub = error "todo: xpub"

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
