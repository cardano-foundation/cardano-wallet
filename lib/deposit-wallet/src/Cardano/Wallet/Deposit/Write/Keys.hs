{-# LANGUAGE DataKinds #-}

-- | Module for converting key types from
-- @Cardano.Ledger@  with key types from @Cardano.Crypto.Wallet@.
--
-- TODO: Match this up with the @Write@ hierarchy.
module Cardano.Wallet.Deposit.Write.Keys
    ( enterpriseAddressFromVKey
    , vkeyFromXPub
    , signedDSIGNfromXSignature
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( xpubPublicKey
    )
import Cardano.Ledger.Keys
    ( SignedDSIGN
    , VKey (..)
    )
import Cardano.Wallet.Address.BIP32_Ed25519
    ( XPub
    , XSignature
    , rawSerialiseXSignature
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Data.Maybe
    ( fromMaybe
    )

import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Ledger.Address as L
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Credential as L
import qualified Cardano.Ledger.Hashes as L
import qualified Cardano.Ledger.Keys as L

{-----------------------------------------------------------------------------
    Key conversion
------------------------------------------------------------------------------}
-- | Create an enterprise address from a ledger 'VKey'.
enterpriseAddressFromVKey
    :: L.Network
    -> VKey 'L.Witness L.StandardCrypto
    -> Address
enterpriseAddressFromVKey network =
    mkEnterpriseAddress
    . L.coerceKeyRole
    . L.hashKey
  where
    mkEnterpriseAddress h =
        L.compactAddr
        $ L.Addr network (L.KeyHashObj h) L.StakeRefNull

-- | Convert 'XPub' to a ledger verification key.
vkeyFromXPub :: XPub -> VKey 'L.Witness L.StandardCrypto
vkeyFromXPub =
    VKey
    . fromMaybe impossible
    . DSIGN.rawDeserialiseVerKeyDSIGN
    . xpubPublicKey
  where
    impossible = error "impossible: Cannot convert XPub to VKey"

-- | Convert 'XSignature' to a ledger signature.
signedDSIGNfromXSignature
    :: XSignature
    -> SignedDSIGN L.StandardCrypto
        (L.Hash L.StandardCrypto L.EraIndependentTxBody)
signedDSIGNfromXSignature =
    DSIGN.SignedDSIGN
    . fromMaybe impossible
    . DSIGN.rawDeserialiseSigDSIGN
    . rawSerialiseXSignature
  where
    impossible = error "impossible: Cannot convert XSignature to SignedDSIGN"
