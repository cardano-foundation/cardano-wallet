module Cardano.Wallet.Spec.Data.Wallet where

import Cardano.Wallet.Spec.Data.AdaBalance
    ( AdaBalance )
import Cardano.Wallet.Spec.Data.WalletId
    ( WalletId )
import Cardano.Wallet.Spec.Data.WalletName
    ( WalletName )

data Wallet = Wallet
    { walletId :: WalletId
    , walletName :: WalletName
    , walletBalance :: AdaBalance
    }
    deriving stock (Show, Eq, Ord)
