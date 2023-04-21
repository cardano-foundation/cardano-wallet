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
    ) where

import Prelude

import Cardano.Wallet.Address.Derivation
    ( BoundedAddressLength (..) )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey )
import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Address.Derivation.Shared
    ( SharedKey )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Data.Function
    ( on )
import Data.Proxy
    ( Proxy (..) )

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
