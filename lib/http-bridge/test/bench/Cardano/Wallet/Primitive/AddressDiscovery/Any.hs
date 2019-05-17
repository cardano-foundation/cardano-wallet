{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Custom address discovery schemes used for testing and benchmarking.
--

module Cardano.Wallet.Primitive.AddressDiscovery.Any
    ( AnyAddressState (..)
    , initAnyState
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..), WalletId (..), WalletName (..) )
import Control.DeepSeq
    ( NFData )
import Crypto.Hash
    ( hash )
import Data.Digest.CRC32
    ( crc32 )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Data.ByteString.Char8 as B8

----------------------------------------------------------------------------

-- | Any Address Derivation
--
-- An arbitrary fraction of addreses are recognized as "ours". This is done by
-- looking at a checksum of the address.
newtype AnyAddressState = AnyAddressState
    { oursProportion :: Double
    }
    deriving stock (Generic, Show)

instance NFData AnyAddressState

instance IsOurs AnyAddressState where
    isOurs (Address addr) s@(AnyAddressState p) = (crc32 addr < p', s)
        where
          p' = floor (fromIntegral (maxBound :: Word32) * p)

instance IsOwned AnyAddressState where
    isOwned _ _ _ = Nothing

instance GenChange AnyAddressState where
    genChange _ = error
        "GenChange.genChange: trying to generate change for \
        \an incompatible scheme 'AnyAddressState'. Please don't."

instance CompareDiscovery AnyAddressState where
    compareDiscovery _ _ _ = error
        "CompareDiscovery.compareDiscovery: trying to generate change for \
        \an incompatible scheme 'AnyAddressState'. Please don't."

instance KnownAddresses AnyAddressState where
    knownAddresses _ = error
        "KnownAddresses.knownAddresses: trying to generate change for \
        \an incompatible scheme 'AnyAddressState'. Please don't."

initAnyState :: Text -> Double -> (WalletId, WalletName, AnyAddressState)
initAnyState wname p = (walletId cfg, WalletName wname, cfg)
  where cfg = AnyAddressState p

walletId :: Show a => a -> WalletId
walletId = WalletId . hash . B8.pack . show
