module Cardano.Wallet.Spec.Network.Manual where

import Cardano.Wallet.Cli.Launcher
    ( WalletApi (..) )
import Cardano.Wallet.Spec.Network.Config
    ( NetworkConfig (..) )

nodeWalletSetup :: (NetworkConfig -> IO ()) -> IO ()
nodeWalletSetup withNetworkConfig = do
    withNetworkConfig
        NetworkConfig
            { networkConfigWallet =
                WalletApi
                    { walletInstanceApiUrl = "http://localhost:8090/v2"
                    , walletInstanceApiHost = "localhost"
                    , walletInstanceApiPort = 8090
                    }
            }
