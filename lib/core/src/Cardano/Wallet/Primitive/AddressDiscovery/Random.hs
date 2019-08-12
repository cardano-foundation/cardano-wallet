{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import Cardano.Byron.Codec.Cbor
    ( decodeAddressDerivationPath, decodeAddressPayload, deserialiseCbor )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), XPrv )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( join )
import Data.Maybe
    ( isJust )
import GHC.Generics
    ( Generic )

newtype RndState = RndState { getRndState :: RndKey 'RootK XPrv }
    deriving (Generic)

instance NFData RndState

-- An address is considered to belong to the 'RndState' wallet if it can be decoded
-- as a Byron HD random address, and where the wallet key can be used to decrypt
-- the address derivation path.
instance IsOurs RndState where
    isOurs (Address addr) st@(RndState key) = (isJust path, st)
      where
        pwd = payloadPassphrase key
        path = do
            payload <- deserialiseCbor decodeAddressPayload addr
            join $ deserialiseCbor (decodeAddressDerivationPath pwd) payload

instance IsOwned RndState RndKey where
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
