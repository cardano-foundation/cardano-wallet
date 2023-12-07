{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.Wallet.Address.Keys.BoundedAddressLength
    ( maxLengthAddressFor
    )
    where

import Cardano.Wallet.Address.Constants
    ( maxLengthAddressForByron
    , maxLengthAddressForIcarus
    , maxLengthAddressForShelley
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address
    )

-- | Returns the longest address that the wallet can generate for a given
--   key.
--
-- This is useful in situations where we want to compute some function of
-- an output under construction (such as a minimum UTxO value), but don't
-- yet have convenient access to a real address.
--
-- Please note that this address should:
--
--  - never be used for anything besides its length and validity properties.
--  - never be used as a payment target within a real transaction.
maxLengthAddressFor
    :: KeyFlavorS k
    -- ^ The key flavor
    -> Address
maxLengthAddressFor keyS = case keyS of
    ByronKeyS -> maxLengthAddressForByron
    IcarusKeyS -> maxLengthAddressForIcarus
    ShelleyKeyS -> maxLengthAddressForShelley
    SharedKeyS -> maxLengthAddressForShelley
