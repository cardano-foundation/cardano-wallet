module Cardano.Wallet.Spec
    ( environmentSpec
    , effectsSpecs
    , walletSpec
    ) where

import Cardano.Wallet.Spec.Interpreters.Effectfully
    ( story )
import Cardano.Wallet.Spec.Stories.Timeouts
    ( timeoutsSpec )
import Cardano.Wallet.Spec.Stories.Wallet
    ( createdWalletHasZeroAda
    , createdWalletListed
    , createdWalletRetrievable
    , testEnvironmentIsReady
    )
import Test.Syd
    ( Spec, describe, sequential )

environmentSpec :: Spec
environmentSpec = describe "Test execution environment" do
    story "Wallet serves network info" testEnvironmentIsReady

walletSpec :: Spec
walletSpec = describe "Wallet Backend API" $ sequential do
    story "Created wallet is listed" createdWalletListed
    story "Created wallet can be retrieved by id" createdWalletRetrievable
    story "Created wallet has zero ADA balance" createdWalletHasZeroAda

effectsSpecs :: Spec
effectsSpecs = describe "Effect interpreters" do
    timeoutsSpec
