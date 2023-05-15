{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Interface over keys / address types
module Cardano.Wallet.Address.Keys.WalletKey
    ( liftRawKeyNew
    , getRawKeyNew
    , keyTypeDescriptorNew
    , digestNew
    , publicKeyNew
    , changePassphraseNew
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Crypto.Wallet
    ( XPub, toXPub, unXPub )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey (..), byronKey, hdPassphrase )
import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey (..), icarusKey )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey (..), sharedKey )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..), shelleyKey )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..) )
import Cardano.Wallet.Primitive.Passphrase
    ( PassphraseScheme, changePassphraseXPrv )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..) )
import Cardano.Wallet.TypeLevel
    ( Excluding )
import Control.Lens
    ( over, view, (^.) )
import Crypto.Hash
    ( Digest, HashAlgorithm, hash )

-- | Re-encrypt a private key using a different passphrase.
--
-- **Important**:
-- This function doesn't check that the old passphrase is correct! Caller is
-- expected to have already checked that. Using an incorrect passphrase here
-- will lead to very bad thing.
changePassphraseNew
    :: KeyFlavorS key
    -- ^ The type of key to serialize.
    -> (PassphraseScheme, Passphrase "user")
        -- ^ Old passphrase.
    -> (PassphraseScheme, Passphrase "user")
        -- ^ New passphrase.
    -> key depth XPrv
    -- ^ Old private key.
    -> key depth XPrv
changePassphraseNew = \case
    ByronKeyS -> \old new key ->
        let masterKey = changePassphraseXPrv old new $ key ^. byronKey
        in ByronKey
            { getKey = masterKey
            , derivationPath = derivationPath key
            , payloadPassphrase = hdPassphrase (toXPub masterKey)
            }
    IcarusKeyS -> \old new -> over icarusKey $ changePassphraseXPrv old new
    ShelleyKeyS -> \old new -> over shelleyKey $ changePassphraseXPrv old new
    SharedKeyS -> \old new -> over sharedKey $ changePassphraseXPrv old new

-- | Extract the public key part of a private key.
publicKeyNew
    :: KeyFlavorS key
    -- ^ The type of key to serialize.
    -> key depth XPrv
    -- ^ Private key.
    -> key depth XPub
publicKeyNew = \case
    ByronKeyS -> over byronKey toXPub
    IcarusKeyS ->  over icarusKey toXPub
    ShelleyKeyS -> over shelleyKey toXPub
    SharedKeyS -> over sharedKey toXPub

-- | Hash a public key to some other representation.
digestNew
    :: HashAlgorithm a
    => KeyFlavorS key
    -- ^ The type of key to serialize.
    -> key depth XPub
    -- ^ Public key.
    -> Digest a
digestNew k = hash . unXPub . getRawKeyNew k

-- | Get a short, human-readable string descriptor that uniquely identifies
--   the specified key type.
keyTypeDescriptorNew :: KeyFlavorS k
    -- ^ The type of key to serialize.
    -> String
keyTypeDescriptorNew = \case
    ByronKeyS -> "rnd"
    IcarusKeyS -> "ica"
    ShelleyKeyS -> "she"
    SharedKeyS -> "sha"

-- | Unwrap the 'WalletKey' to use the 'XPrv' or 'XPub'.
getRawKeyNew
    :: KeyFlavorS key
    -- ^ The type of key to serialize.
    -> key depth raw
    -- ^ The key to unwrap, public or private.
    -> raw
getRawKeyNew = \case
    ByronKeyS -> view byronKey
    IcarusKeyS -> view icarusKey
    ShelleyKeyS -> view shelleyKey
    SharedKeyS -> view sharedKey

type AfterByron k = Excluding '[ByronKey] k

-- | Lift 'XPrv' or 'XPub' to 'WalletKey'.
liftRawKeyNew
    :: AfterByron key
    => KeyFlavorS key
    -- ^ The type of key to serialize.
    -> raw
    -- ^ The key to unwrap, public or private.
    -> key depth raw
liftRawKeyNew = \case
    IcarusKeyS -> IcarusKey
    ShelleyKeyS -> ShelleyKey
    SharedKeyS -> SharedKey
