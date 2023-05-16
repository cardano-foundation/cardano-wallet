{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Flavor
    ( WalletFlavorS (..)
    , WalletFlavor (..)
    , KeyOf
    , TestState (..)
    )
where

import Prelude

import Cardano.Wallet.Address.Derivation
    ( Depth )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey )
import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey (..) )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Address.Discovery.Random
    ( RndAnyState, RndState (..) )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqAnyState, SeqState )
import Cardano.Wallet.Address.Discovery.Shared
    ( SharedState (..) )
import Data.Kind
    ( Type )
import GHC.Generics
    ( Generic )

data WalletFlavorS s where
    ShelleyWallet :: WalletFlavorS (SeqState n ShelleyKey)
    IcarusWallet :: WalletFlavorS (SeqState n IcarusKey)
    ByronWallet :: WalletFlavorS (RndState n)
    SharedWallet :: WalletFlavorS (SharedState n SharedKey)
    BenchByronWallet :: WalletFlavorS (RndAnyState n p)
    BenchShelleyWallet :: WalletFlavorS (SeqAnyState n ShelleyKey p)

class WalletFlavor s where
    walletFlavor :: WalletFlavorS s

instance WalletFlavor (SeqState n IcarusKey) where
    walletFlavor = IcarusWallet

instance WalletFlavor (SeqState n ShelleyKey) where
    walletFlavor = ShelleyWallet

instance WalletFlavor (RndState n) where
    walletFlavor = ByronWallet

instance WalletFlavor (SeqAnyState n ShelleyKey p) where
    walletFlavor = BenchShelleyWallet

instance WalletFlavor (RndAnyState n p) where
    walletFlavor = BenchByronWallet

instance WalletFlavor (SharedState n SharedKey) where
    walletFlavor = SharedWallet

newtype TestState s (k :: (Depth -> Type -> Type)) = TestState s
    deriving (Generic, Show, Eq)

type family KeyOf (s :: Type) :: (Depth -> Type -> Type) where
    KeyOf (SeqState n k) = k
    KeyOf (RndState n) = ByronKey
    KeyOf (SharedState n k) = k
    KeyOf (SeqAnyState n k p) = k
    KeyOf (RndAnyState n p) = ByronKey
    KeyOf (TestState s k) = k
