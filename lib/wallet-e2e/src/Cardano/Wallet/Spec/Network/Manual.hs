module Cardano.Wallet.Spec.Network.Manual where

import Cardano.Wallet.Spec.Network.Config
    ( NetworkConfig (..) )
import Cardano.Wallet.Spec.Network.Wallet
    ( WalletApi (..) )

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
