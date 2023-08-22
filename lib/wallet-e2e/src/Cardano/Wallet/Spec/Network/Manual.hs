module Cardano.Wallet.Spec.Network.Manual where

import Cardano.Wallet.Spec.Network.Config
    ( NetworkConfig )

nodeWalletSetup :: (NetworkConfig -> IO ()) -> IO ()
nodeWalletSetup f = do
    f ()
