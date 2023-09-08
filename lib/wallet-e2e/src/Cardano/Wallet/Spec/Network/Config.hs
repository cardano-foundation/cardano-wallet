module Cardano.Wallet.Spec.Network.Config where

import Cardano.Wallet.Cli.Launcher
    ( WalletApi )

newtype NetworkConfig = NetworkConfig {networkConfigWallet :: WalletApi}
