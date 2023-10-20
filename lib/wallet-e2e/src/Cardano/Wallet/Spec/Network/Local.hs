module Cardano.Wallet.Spec.Network.Local
    ( nodeWalletSetup
    ) where

import Cardano.Wallet.Spec.Network.Config
    ( NetworkConfig (..)
    )
import Path
    ( Abs
    , Dir
    , Path
    )

nodeWalletSetup :: Path Abs Dir -> (NetworkConfig -> IO ()) -> IO ()
nodeWalletSetup _stateDir withNetworkConfig = do
    putTextLn "TODO: implement nodeWalletSetup for a local test cluster"
    withNetworkConfig NetworkConfig{networkConfigWallet = error "TODO"}
