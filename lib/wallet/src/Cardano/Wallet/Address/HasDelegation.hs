{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Address.HasDelegation
    (
    HasDelegation(..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )

-- | Discriminate if a particular state supports delegation
class HasDelegation s where
    hasDelegation :: p s -> Bool

instance HasDelegation (SeqState n ShelleyKey) where
    hasDelegation _ = True

instance HasDelegation (RndState n) where
    hasDelegation _ = False

instance HasDelegation (SeqState n IcarusKey) where
    hasDelegation _ = False
