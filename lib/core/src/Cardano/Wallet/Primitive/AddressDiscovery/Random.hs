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
-- License: Apache-2.0
--
-- An implementation of address discovery for the random address
-- scheme as used by the legacy Cardano wallets.

module Cardano.Wallet.Primitive.AddressDiscovery.Random
    (
    -- ** State
      RndState (..)
    , emptyChangeState
    , updateChangeState
    , ChangeState (..)
    ) where

import Prelude

import Cardano.Byron.Codec.Cbor
    ( decodeAddressDerivationPath, decodeAddressPayload, deserialiseCbor )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), DerivationType (..), Index (..), XPrv )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey (..), deriveAccountPrivateKey, deriveAddressPrivateKey )
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
import Data.Set
    ( Set )
import GHC.Generics
    ( Generic )

import qualified Data.Set as Set

data RndState = RndState
    { rndKey :: RndKey 'RootK XPrv
    , changeState :: ChangeState
    } deriving (Generic)

instance NFData RndState

-- An address is considered to belong to the 'RndState' wallet if it can be decoded
-- as a Byron HD random address, and where the wallet key can be used to decrypt
-- the address derivation path.
instance IsOurs RndState where
    isOurs addr st@(RndState key _) =
        (isJust $ addressToPath addr key, st)

instance IsOwned RndState RndKey where
    isOwned (RndState key _) (_,pwd) addr =
        case addressToPath addr key of
            Just (accIx, addrIx) -> do
                let accXPrv = deriveAccountPrivateKey pwd key accIx
                let addrXPrv = deriveAddressPrivateKey pwd accXPrv addrIx
                Just (addrXPrv, pwd)
            _ -> Nothing

addressToPath
    :: Address
    -> RndKey 'RootK XPrv
    -> Maybe (Index 'Hardened 'AccountK, Index 'Hardened 'AddressK)
addressToPath (Address addr) key = do
    let pwd = payloadPassphrase key
    payload <- deserialiseCbor decodeAddressPayload addr
    join $ deserialiseCbor (decodeAddressDerivationPath pwd) payload

-- | Change state keeps track of the first free index to use and
-- already used addresses (not only change addresses but ever known, ie. generated)
data ChangeState = ChangeState
    { forbidenAddresses :: Set Address
    , nextIndex :: (Index 'Soft 'AccountK, Index 'Soft 'AddressK)
    } deriving (Generic, Show, Eq)

instance NFData ChangeState

emptyChangeState :: ChangeState
emptyChangeState = ChangeState Set.empty (minBound, minBound)

updateChangeState
    :: Address
    -> ChangeState
    -> ChangeState
updateChangeState addr (ChangeState addrs (Index accIx, Index addrIx)) =
    let (accIx', addrIx') =
            if (addrIx == maxBound && accIx == maxBound) then
                (minBound, minBound)
            else if (addrIx == maxBound) then
                (accIx + 1, minBound)
            else (accIx, addrIx + 1)
    in ChangeState (Set.insert addr addrs) (Index accIx', Index addrIx')


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
