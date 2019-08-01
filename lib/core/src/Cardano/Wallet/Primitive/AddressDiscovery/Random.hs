{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
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
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Key, XPrv, publicKey )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( addrToPayload, decodeAddressDerivationPath, deserialise )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Control.DeepSeq
    ( NFData )
import GHC.Generics
    ( Generic )

newtype RndState = RndState { getRndState :: Key 'RootK XPrv }
    deriving (Generic)

instance NFData RndState

instance IsOurs RndState where
    isOurs addr (RndState s) = do
        case deserialise (decodeAddressDerivationPath $ publicKey s) (addrToPayload addr) of
            Right (Just _) -> (True, RndState s)
            _ -> (False, RndState s)

instance IsOwned RndState where
    isOwned _ _ _ = Nothing

instance GenChange RndState where
    genChange s = (error "GenChange RndState unimplemented", s)

-- Unlike sequential derivation, we can't derive an order from the index only
-- (they are randomly generated), nor anything else in the address itself.
--
-- Therefore, we'll simply consider that addresses using the random address
-- derivation scheme won't be ordered in any particular order.
instance CompareDiscovery RndState where
    compareDiscovery _ _ _ = EQ

instance KnownAddresses RndState where
    knownAddresses _ = []
