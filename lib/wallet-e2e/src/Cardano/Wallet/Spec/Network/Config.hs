module Cardano.Wallet.Spec.Network.Config where

import Cardano.Wallet.Spec.Network.Wallet
    ( WalletApi )

newtype NetworkConfig = NetworkConfig {networkConfigWallet :: WalletApi}
