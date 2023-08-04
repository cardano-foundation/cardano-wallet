{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Provides various 'Address' constants used by the wallet or its tests.
module Cardano.Wallet.Primitive.Types.Address.Constants
  ( maxLengthAddress
  )
where

import Cardano.Wallet.Address.Keys.BoundedAddressLength
  ( maxLengthAddressFor
  )
import Cardano.Wallet.Flavor
  ( KeyFlavorS (..)
  )
import Cardano.Wallet.Primitive.Types.Address
  ( Address (..)
  )
import Data.ByteString qualified as BS
import Data.Function
  ( on
  )
import Data.List qualified as L
import Prelude

-- | A dummy 'Address' of the greatest length that the wallet can generate.
--
-- Please note that this address should:
--
--  - never be used for anything besides its length and validity properties.
--  - never be used as a payment target within a real transaction.
maxLengthAddress :: Address
maxLengthAddress =
  L.maximumBy
    (compare `on` (BS.length . unAddress))
    [ maxLengthAddressFor ByronKeyS
    , maxLengthAddressFor IcarusKeyS
    , maxLengthAddressFor ShelleyKeyS
    , maxLengthAddressFor SharedKeyS
    ]
