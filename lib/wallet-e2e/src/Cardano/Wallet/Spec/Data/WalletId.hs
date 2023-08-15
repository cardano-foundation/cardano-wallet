module Cardano.Wallet.Spec.Data.WalletId
    ( WalletId
    , walletIdToText
    , mkUnsafe
    , fromNel
    ) where

newtype WalletId = WalletId {walletIdToText :: Text}
    deriving stock (Show, Eq, Ord)

fromNel :: NonEmpty Text -> WalletId
fromNel = WalletId . unwords . toList

mkUnsafe :: Text -> WalletId
mkUnsafe = WalletId
