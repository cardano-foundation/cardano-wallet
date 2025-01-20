module Cardano.Wallet.Spec
    ( TestNetworkConfig (..)
    , configureTestNet
    ) where

import qualified Cardano.Wallet.Spec.Network.Local as Local
import qualified Cardano.Wallet.Spec.Network.Manual as Manual
import qualified Cardano.Wallet.Spec.Network.Preprod as Preprod

import Cardano.Wallet.Launch.Cluster.FileOf
    ( DirOf
    )
import Cardano.Wallet.Spec.Network.Configured
    ( ConfiguredNetwork
    )
import Control.Monad.Trans.Resource
    ( runResourceT
    )

--------------------------------------------------------------------------------
-- Test network setup ----------------------------------------------------------

data TestNetworkConfig
    = TestNetworkManual
    | TestNetworkLocal (DirOf "state") (DirOf "config")
    | TestNetworkPreprod (DirOf "state") (DirOf "config")
    deriving stock Show

configureTestNet :: TestNetworkConfig -> (ConfiguredNetwork -> IO ()) -> IO ()
configureTestNet testNetworkConfig withConfiguredNetwork = runResourceT $ do
    config <- case testNetworkConfig of
        TestNetworkManual ->
            pure Manual.configuredNetwork
        TestNetworkLocal stateDir nodeConfigDir ->
            Local.configuredNetwork stateDir nodeConfigDir Nothing
        TestNetworkPreprod stateDir nodeConfigDir ->
            Preprod.configuredNetwork stateDir nodeConfigDir
    liftIO $ withConfiguredNetwork config
