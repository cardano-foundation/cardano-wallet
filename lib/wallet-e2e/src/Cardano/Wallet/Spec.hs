module Cardano.Wallet.Spec
    ( walletSpec
    , effectsSpec
    , TestNetworkConfig (..)
    ) where

import qualified Cardano.Wallet.Spec.Network.Local as Local
import qualified Cardano.Wallet.Spec.Network.Manual as Manual
import qualified Cardano.Wallet.Spec.Network.Preprod as Preprod

import Cardano.Wallet.Spec.Interpreters.Effectfully
    ( story )
import Cardano.Wallet.Spec.Network.Config
    ( NetworkConfig )
import Cardano.Wallet.Spec.Stories.Wallet
    ( createdWalletHasZeroAda, createdWalletListed, createdWalletRetrievable )
import Cardano.Wallet.Spec.TimeoutSpec
    ( timeoutSpec )
import Path
    ( Abs, Dir, Path )
import Test.Syd
    ( Spec, aroundAll, describe, sequential )

walletSpec :: TestNetworkConfig -> Spec
walletSpec networkConfig =
    aroundAll (setupForNetwork networkConfig) do
        describe "Wallet Backend API" $ sequential do
            story "Created wallet is listed" createdWalletListed
            story "Created wallet can be retrieved by id" createdWalletRetrievable
            story "Created wallet has zero ADA balance" createdWalletHasZeroAda

effectsSpec :: Spec
effectsSpec = describe "Effect interpreters" do
    timeoutSpec

--------------------------------------------------------------------------------
-- Test network setup ----------------------------------------------------------

data TestNetworkConfig
    = TestNetworkManual
    | TestNetworkLocal !(Path Abs Dir)
    | TestNetworkPreprod !(Path Abs Dir) !(Path Abs Dir)

setupForNetwork :: TestNetworkConfig -> (NetworkConfig -> IO ()) -> IO ()
setupForNetwork = \case
    TestNetworkManual ->
        Manual.nodeWalletSetup
    TestNetworkLocal stateDir ->
        Local.nodeWalletSetup stateDir
    TestNetworkPreprod stateDir nodeConfigDir ->
        Preprod.nodeWalletSetup stateDir nodeConfigDir
