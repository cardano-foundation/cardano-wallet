module Cardano.Wallet.Spec
    ( environmentSpec
    , walletSpec
    ) where

import Cardano.Wallet.Spec.Interpreters.Effectfully
    ( story )
import Cardano.Wallet.Spec.Stories.Wallet
    ( createdWalletListed, createdWalletRetrievable, testEnvironmentIsReady )
import Test.Syd
    ( Spec, describe, sequential )

environmentSpec :: Spec
environmentSpec = describe "Test execution environment" do
    story "Wallet serves network info" testEnvironmentIsReady

walletSpec :: Spec
walletSpec = describe "Wallet Backend API" $ sequential do
    story "Created wallet is listed" createdWalletListed
    story "Created wallet can be retrieved by id" createdWalletRetrievable
