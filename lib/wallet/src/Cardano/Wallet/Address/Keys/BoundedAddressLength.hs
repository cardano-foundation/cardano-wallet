{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Cardano.Wallet.Address.Keys.BoundedAddressLength
  ( maxLengthAddressFor
  )
where

import Cardano.Byron.Codec.Cbor qualified as CBOR
import Cardano.Crypto.Wallet qualified as CC
import Cardano.Wallet.Flavor
  ( KeyFlavorS (..)
  )
import Cardano.Wallet.Primitive.Passphrase.Types
  ( Passphrase (..)
  )
import Cardano.Wallet.Primitive.Types.Address
  ( Address (Address)
  )
import Cardano.Wallet.Primitive.Types.ProtocolMagic
  ( ProtocolMagic (ProtocolMagic)
  )
import Codec.CBOR.Write qualified as CBOR
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Prelude

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

maxLengthAddressForByron :: Address
maxLengthAddressForByron =
  Address
    $ CBOR.toStrictByteString
    $ CBOR.encodeAddress
      xpub
      [ CBOR.encodeDerivationPathAttr passphrase maxBound maxBound
      , CBOR.encodeProtocolMagicAttr (ProtocolMagic maxBound)
      ]
  where
    -- Must apparently always be 32 bytes:
    passphrase :: Passphrase "addr-derivation-payload"
    passphrase = Passphrase $ BA.convert $ BS.replicate 32 0

    xpub :: CC.XPub
    xpub = CC.toXPub $ CC.generate (BS.replicate 32 0) xprvPass
      where
        xprvPass = mempty :: BS.ByteString

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
