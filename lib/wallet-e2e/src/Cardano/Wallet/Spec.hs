module Cardano.Wallet.Spec
    ( walletSpec
    , effectsSpec
    ) where

import qualified Cardano.Wallet.Spec.Network.Local as Local

-- import qualified Cardano.Wallet.Spec.Network.Manual as Manual

import Cardano.Wallet.Spec.Interpreters.Effectfully
    ( story )
import Cardano.Wallet.Spec.Stories.Wallet
    ( createdWalletHasZeroAda, createdWalletListed, createdWalletRetrievable )
import Cardano.Wallet.Spec.TimeoutSpec
    ( timeoutSpec )
import Path
import Test.Syd
    ( Spec, aroundAll, describe, sequential )

walletSpec :: Path Abs Dir -> Spec
walletSpec stateDirectory =
    aroundAll (Local.nodeWalletSetup stateDirectory) do
        describe "Wallet Backend API" $ sequential do
            story "Created wallet is listed" createdWalletListed
            story "Created wallet can be retrieved by id" createdWalletRetrievable
            story "Created wallet has zero ADA balance" createdWalletHasZeroAda

effectsSpec :: Spec
effectsSpec = describe "Effect interpreters" do
    timeoutSpec
