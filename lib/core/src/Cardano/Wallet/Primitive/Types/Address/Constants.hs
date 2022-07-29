{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Provides various 'Address' constants used by the wallet or its tests.
--
module Cardano.Wallet.Primitive.Types.Address.Constants
    ( maxLengthAddress
    , minLengthAddress
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( BoundedAddressLength (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shared
    ( SharedKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Data.Proxy
    ( Proxy (..) )

import Data.Function
    ( on )

import qualified Data.ByteString as BS
import qualified Data.List as L

-- | A dummy 'Address' of the greatest length that the wallet can generate.
--
-- Please note that this address should:
--
--  - never be used for anything besides its length and validity properties.
--  - never be used as a payment target within a real transaction.
--
maxLengthAddress :: Address
maxLengthAddress = L.maximumBy (compare `on` (BS.length . unAddress))
    [ maxLengthAddressFor $ Proxy @ByronKey
    , maxLengthAddressFor $ Proxy @IcarusKey
    , maxLengthAddressFor $ Proxy @ShelleyKey
    , maxLengthAddressFor $ Proxy @SharedKey
    ]

-- | A dummy 'Address' of the shortest length that the wallet can generate.
--
-- Please note that this address should:
--
--  - never be used for anything besides its length and validity properties.
--  - never be used as a payment target within a real transaction.
--
minLengthAddress :: Address
minLengthAddress = minLengthAddressShelley
  where
    minLengthAddressShelley =
        Address $ BS.singleton enterpriseAddressHeaderByte <> payload
      where
        enterpriseAddressHeaderByte = 0b01100000
        payload = BS.replicate 28 0
