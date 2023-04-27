{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Wallet.Flavor (WalletFlavorS (..), WalletFlavor (..))
    where

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

data WalletFlavorS s n k  where
    ShelleyWallet :: WalletFlavorS (SeqState n ShelleyKey) n ShelleyKey
    IcarusWallet :: WalletFlavorS (SeqState n IcarusKey) n IcarusKey
    ByronWallet :: WalletFlavorS (RndState n) n ByronKey
    SharedWallet :: WalletFlavorS (SharedState n k) n SharedKey
    BenchByronWallet :: WalletFlavorS (RndAnyState n p) n ByronKey
    BenchShelleyWallet :: WalletFlavorS (SeqAnyState n ShelleyKey p) n ShelleyKey

class WalletFlavor s n k where
    walletFlavor :: WalletFlavorS s n k

instance WalletFlavor (SeqState n IcarusKey) n IcarusKey where
    walletFlavor = IcarusWallet

instance WalletFlavor (SeqState n ShelleyKey) n ShelleyKey where
    walletFlavor = ShelleyWallet

instance WalletFlavor (RndState n) n ByronKey where
    walletFlavor = ByronWallet

instance WalletFlavor (SeqAnyState n ShelleyKey p) n ShelleyKey where
    walletFlavor = BenchShelleyWallet

instance WalletFlavor (RndAnyState n p) n ByronKey where
    walletFlavor = BenchByronWallet

instance WalletFlavor (SharedState n SharedKey) n SharedKey where
    walletFlavor = SharedWallet
