module Cardano.Wallet.Spec.Network.Configured where

import Cardano.Wallet.Cli.Launcher
    ( WalletApi
    )

newtype ConfiguredNetwork = ConfiguredNetwork
  { configuredNetworkWallet :: WalletApi
  }
