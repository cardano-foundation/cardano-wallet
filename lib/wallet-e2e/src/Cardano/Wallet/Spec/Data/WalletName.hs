module Cardano.Wallet.Spec.Data.WalletName
    ( WalletName
    , walletNameToText
    , mkUnsafe
    ) where

newtype WalletName = WalletName {walletNameToText :: Text}
    deriving stock (Show, Eq, Ord)

mkUnsafe :: Text -> WalletName
mkUnsafe = WalletName
