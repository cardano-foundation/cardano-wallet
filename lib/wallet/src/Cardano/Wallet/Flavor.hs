{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Flavor
    ( WalletFlavorS (..)
    , WalletFlavor (..)
    , KeyOf
    , TestState (..)
    , KeyFlavorS (..)
    , keyFlavor
    , Including
    , Excluding
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

-- | A singleton type to capture the flavor of a state.
data WalletFlavorS s where
    ShelleyWallet :: WalletFlavorS (SeqState n ShelleyKey)
    IcarusWallet :: WalletFlavorS (SeqState n IcarusKey)
    ByronWallet :: WalletFlavorS (RndState n)
    SharedWallet :: WalletFlavorS (SharedState n SharedKey)
    BenchByronWallet :: WalletFlavorS (RndAnyState n p)
    BenchShelleyWallet :: WalletFlavorS (SeqAnyState n ShelleyKey p)
    TestStateS :: WalletFlavorS (TestState s ShelleyKey)

-- | A function to reify the flavor of a state.
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

instance WalletFlavor (TestState n ShelleyKey) where
    walletFlavor = TestStateS

-- | A type for states that will be used in tests.
newtype TestState s (k :: (Depth -> Type -> Type)) = TestState s
    deriving (Generic, Show, Eq)

-- | A type family to get the key type from a state.
type family KeyOf (s :: Type) :: (Depth -> Type -> Type) where
    KeyOf (SeqState n k) = k
    KeyOf (RndState n) = ByronKey
    KeyOf (SharedState n k) = k
    KeyOf (SeqAnyState n k p) = k
    KeyOf (RndAnyState n p) = ByronKey
    KeyOf (TestState s k) = k

-- | A singleton type to capture the flavor of a key.
data KeyFlavorS a where
    ByronKeyS :: KeyFlavorS ByronKey
    IcarusKeyS :: KeyFlavorS IcarusKey
    ShelleyKeyS :: KeyFlavorS ShelleyKey
    SharedKeyS :: KeyFlavorS SharedKey

-- | Map a wallet flavor to a key flavor.
keyOfWallet :: WalletFlavorS s -> KeyFlavorS (KeyOf s)
keyOfWallet ShelleyWallet = ShelleyKeyS
keyOfWallet IcarusWallet = IcarusKeyS
keyOfWallet ByronWallet = ByronKeyS
keyOfWallet SharedWallet = SharedKeyS
keyOfWallet BenchByronWallet = ByronKeyS
keyOfWallet BenchShelleyWallet = ShelleyKeyS
keyOfWallet TestStateS = ShelleyKeyS

-- | A function to reify the flavor of a key from a state type.
--
-- use with
-- > keyFlavor @s
keyFlavor :: forall s. WalletFlavor s => KeyFlavorS (KeyOf s)
keyFlavor = keyOfWallet (walletFlavor @s)

type family Exclude xs x where
    Exclude '[] _ = 'True
    Exclude (x ': xs) x = 'False
    Exclude (x ': xs) y = Exclude xs y

type family Include xs x where
    Include '[] _ = 'False
    Include (x ': xs) x = 'True
    Include (x ': xs) y = Include xs y

type Excluding (xs :: [k]) (x :: k) = Exclude xs x ~ 'True

type Including xs x = Include xs x ~ 'True
