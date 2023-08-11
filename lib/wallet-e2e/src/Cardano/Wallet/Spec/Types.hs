module Cardano.Wallet.Spec.Types where

data Wallet = Wallet
    { walletId :: Text
    , walletName :: Text
    }
    deriving stock (Show, Eq, Ord)

newtype Mnemonic = Mnemonic {mnemonicWords :: NonEmpty Text}
    deriving stock (Show)
    deriving newtype (Eq, Ord)
