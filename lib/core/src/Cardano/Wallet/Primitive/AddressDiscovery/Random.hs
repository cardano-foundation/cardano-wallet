{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- An implementation of address discovery for the random address
-- scheme as used by the legacy Cardano wallets.

module Cardano.Wallet.Primitive.AddressDiscovery.Random
    (
    -- ** State
      RndState (..)
    , mkRndState
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Data.ByteString
    ( ByteString )

newtype RndState = RndState ByteString

mkRndState :: RndState
mkRndState = error "AddressDiscovery.Random unimplemented"

instance IsOurs RndState where
    isOurs _ s = (False, s)

instance IsOwned RndState where
    isOwned _ _ _ = Nothing

instance GenChange RndState where
    genChange s = (error "GenChange RndState unimplemented", s)

instance CompareDiscovery RndState where
    compareDiscovery _ _ _ = EQ

instance KnownAddresses RndState where
    knownAddresses _ = []
