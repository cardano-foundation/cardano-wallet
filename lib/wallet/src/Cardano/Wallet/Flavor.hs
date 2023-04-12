{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Flavor (WalletFlavorS (..), WalletFlavor (..))
    where

import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( SharedState (..) )

data WalletFlavorS s n k  where
    ShelleyWallet :: WalletFlavorS (SeqState n k) n k
    ByronWallet :: WalletFlavorS (RndState n) n k
    SharedWallet :: WalletFlavorS (SharedState n k) n k

class WalletFlavor s n k where
    walletFlavor :: WalletFlavorS s n k

instance WalletFlavor (SeqState n k) n k where
    walletFlavor = ShelleyWallet

instance WalletFlavor (RndState n) n k where
    walletFlavor = ByronWallet

instance WalletFlavor (SharedState n k) n k where
    walletFlavor = SharedWallet
