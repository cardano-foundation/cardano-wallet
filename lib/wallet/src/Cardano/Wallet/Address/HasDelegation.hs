{-# LANGUAGE FlexibleInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Type class that discriminates whether an
-- address state supports delegation.
module Cardano.Wallet.Address.HasDelegation
    ( HasDelegation (..)
    ) where

import Prelude

import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState (..) )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState )
import Cardano.Wallet.Address.Discovery.Shared
    ( SharedState )

-- | Discriminate whether an address state supports delegation.
class HasDelegation s where
    hasDelegation :: proxy s -> Bool

instance HasDelegation (SeqState n ShelleyKey) where
    hasDelegation _ = True

instance HasDelegation (RndState n) where
    hasDelegation _ = False

instance HasDelegation (SeqState n IcarusKey) where
    hasDelegation _ = False

instance HasDelegation (SharedState n SharedKey) where
    hasDelegation _ = False
