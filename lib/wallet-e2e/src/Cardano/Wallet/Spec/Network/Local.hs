module Cardano.Wallet.Spec.Network.Local where

import Cardano.Wallet.Spec.Network.Config
    ( NetworkConfig )

nodeWalletSetup :: (NetworkConfig -> IO ()) -> IO ()
nodeWalletSetup f = do
    f ()
