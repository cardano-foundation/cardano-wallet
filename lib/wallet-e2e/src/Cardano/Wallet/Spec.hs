module Cardano.Wallet.Spec
    ( spec
    )
where

import Cardano.Wallet.Spec.Interpreters.Effectfully
    ( story )
import Cardano.Wallet.Spec.Stories.Wallet
    ( createdWallet )
import Test.Syd
    ( Spec, describe )

spec :: Spec
spec = do
    describe "Wallet Backend API" do
        describe "Wallets" do
            story "Created wallet is known" createdWallet
