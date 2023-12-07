{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.Wallet.Address.Keys.BoundedAddressLength
    ( maxLengthAddressFor
    )
    where

import Prelude

import Cardano.Wallet.Address.Constants
    ( maxLengthAddressForByron
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (Address)
    )
import Cardano.Wallet.Primitive.Types.ProtocolMagic
    ( ProtocolMagic (ProtocolMagic)
    )

import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS

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

maxLengthAddressForIcarus :: Address
maxLengthAddressForIcarus =
    Address
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress
            xpub
            [ CBOR.encodeProtocolMagicAttr (ProtocolMagic maxBound)
            ]
  where
    xpub :: CC.XPub
    xpub = CC.toXPub $ CC.generate (BS.replicate 32 0) xprvPass
      where
        xprvPass = mempty :: BS.ByteString

maxLengthAddressForShelley :: Address
maxLengthAddressForShelley = Address $ BS.replicate 57 0
