module Cardano.Wallet.Spec
    ( walletSpec
    , effectsSpec
    , TestNetworkConfig (..)
    ) where

import qualified Cardano.Wallet.Spec.Network.Local as Local
import qualified Cardano.Wallet.Spec.Network.Manual as Manual
import qualified Cardano.Wallet.Spec.Network.Preprod as Preprod

import Cardano.Wallet.Spec.Interpreters.Config
    ( TraceConfiguration
    )
import Cardano.Wallet.Spec.Interpreters.Effectfully
    ( story
    )
import Cardano.Wallet.Spec.Network.Configured
    ( ConfiguredNetwork
    )
import Cardano.Wallet.Spec.Stories.Wallet
    ( createdWalletHasZeroAda
    , createdWalletListed
    , createdWalletRetrievable
    )
import Cardano.Wallet.Spec.TimeoutSpec
    ( timeoutSpec
    )
import Control.Monad.Trans.Resource
    ( runResourceT
    )
import Data.Tagged
    ( Tagged
    )
import Path
    ( Abs
    , Dir
    , Path
    )
import Test.Syd
    ( Spec
    , aroundAll
    , describe
    , sequential
    )

walletSpec :: TraceConfiguration -> TestNetworkConfig -> Spec
walletSpec tracingConfig config =
    aroundAll (configureTracing tracingConfig)
        $ aroundAll (configureTestNet config)
        $ do
            describe "Wallet Backend API" $ sequential do
                story
                    "Created wallet is listed"
                    createdWalletListed
                story
                    "Created wallet can be retrieved by id"
                    createdWalletRetrievable
                story
                    "Created wallet has zero ADA balance"
                    createdWalletHasZeroAda

configureTracing :: TraceConfiguration -> (TraceConfiguration -> IO ()) -> IO ()
configureTracing config f = f config

effectsSpec :: Spec
effectsSpec = describe "Effect interpreters" do
    timeoutSpec

--------------------------------------------------------------------------------
-- Test network setup ----------------------------------------------------------

data TestNetworkConfig
    = TestNetworkManual
    | TestNetworkLocal
        (Tagged "state" (Path Abs Dir))
        (Tagged "config" (Path Abs Dir))
    | TestNetworkPreprod
        (Tagged "state" (Path Abs Dir))
        (Tagged "config" (Path Abs Dir))

configureTestNet :: TestNetworkConfig -> (ConfiguredNetwork -> IO ()) -> IO ()
configureTestNet testNetworkConfig withConfiguredNetwork = runResourceT $ do
    config <- case testNetworkConfig of
        TestNetworkManual ->
            pure Manual.configuredNetwork
        TestNetworkLocal stateDir nodeConfigDir ->
            Local.configuredNetwork stateDir nodeConfigDir
        TestNetworkPreprod stateDir nodeConfigDir ->
            Preprod.configuredNetwork stateDir nodeConfigDir
    liftIO $ withConfiguredNetwork config
