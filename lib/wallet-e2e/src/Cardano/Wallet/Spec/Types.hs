module Cardano.Wallet.Spec.Types where

newtype Wallet = Wallet Text
    deriving stock (Show)
    deriving newtype (Eq, Ord)

newtype Mnemonic = Mnemonic (NonEmpty Text)
    deriving stock (Show)
    deriving newtype (Eq, Ord)
