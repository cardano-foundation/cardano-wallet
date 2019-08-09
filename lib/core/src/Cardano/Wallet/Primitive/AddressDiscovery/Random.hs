{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    ( decodeAddressDerivationPath, decodeAddressPayload, deserialise )
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
import Cardano.Wallet.Unsafe
    ( unsafeDeserialiseCbor, unsafeFromHex )
import Control.DeepSeq
    ( NFData )
import GHC.Generics
    ( Generic )

import qualified Data.ByteString.Lazy as BL


newtype RndState = RndState { getRndState :: RndKey 'RootK XPrv }
    deriving (Generic)

instance NFData RndState

instance IsOurs RndState where
    isOurs (Address addr) (RndState key) = do
        let payload = unsafeDeserialiseCbor decodeAddressPayload $
                BL.fromStrict (unsafeFromHex addr)
        let pwd = payloadPassphrase key
        case deserialise (decodeAddressDerivationPath pwd) payload of
            Right (Just _) -> (True, RndState key)
            _ -> (False, RndState key)

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
