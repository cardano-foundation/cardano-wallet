module Cardano.Wallet.Spec
    ( spec
    )
where

import Cardano.Wallet.Spec.Interpreters.Pure
    ( pureStory )
import Cardano.Wallet.Spec.Stories.Wallet
    ( createdWallet )
import Test.Syd
    ( Spec, describe )

spec :: Spec
spec = do
    describe "Wallet Backend API" do
        describe "Wallets" do
            pureStory "Created wallet is known" createdWallet
