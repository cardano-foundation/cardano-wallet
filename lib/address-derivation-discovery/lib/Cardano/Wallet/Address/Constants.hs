{-# LANGUAGE DataKinds #-}

module Cardano.Wallet.Address.Constants
    ( maxLengthAddressForByron
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
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
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

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
