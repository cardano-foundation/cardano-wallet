{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Implementation of address derivation for the random scheme, as
-- implemented by the legacy Cardano wallets.
--
-- For full documentation of the key derivation schemes,
-- see the "Cardano.Crypto.Wallet" module, and the implementation in
-- <https://github.com/input-output-hk/cardano-crypto/blob/4590efa638397e952a51a8994b5543e4ea3c1ecd/cbits/encrypted_sign.c cardano-crypto>.

module Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( -- * Types
      ByronKey(..)
    , DerivationPathFrom

      -- * Generation
    , unsafeGenerateKeyFromSeed
    , generateKeyFromSeed
    , minSeedLengthBytes
    , unsafeMkByronKeyFromMasterKey
    , mkByronKeyFromMasterKey

      -- * Derivation
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey

    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( DerivationScheme (DerivationScheme1)
    , XPrv
    , XPub
    , deriveXPrv
    , generate
    , toXPub
    , unXPrv
    , unXPub
    , xprv
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..), entropyToBytes, mnemonicToEntropy )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , ErrMkKeyFingerprint (..)
    , Index (..)
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , PersistPrivateKey (..)
    , WalletKey (..)
    , fromHex
    , hex
    )
import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..)
    , PassphraseHash (..)
    , PassphraseScheme (..)
    , changePassphraseXPrv
    )
import Cardano.Wallet.Primitive.Types
    ( testnetMagic )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Util
    ( invariant )
import Control.DeepSeq
    ( NFData )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( SHA512 (..) )
import Crypto.Hash.Utils
    ( blake2b256 )
import Data.ByteArray
    ( ScrubbedBytes )
import Data.ByteString
    ( ByteString )
import Data.Kind
    ( Type )
import Data.Proxy
    ( Proxy (..) )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat )


import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Cardano.Wallet.Primitive.AddressDerivation as W
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8

{-------------------------------------------------------------------------------
                                   Key Types
-------------------------------------------------------------------------------}

-- | Material for deriving HD random scheme keys, which can be used for making
-- addresses.
data ByronKey (depth :: Depth) key = ByronKey
    { getKey :: key
    -- ^ The raw private or public key.
    , derivationPath :: DerivationPathFrom depth
    -- ^ The address derivation indices for the level of this key.
    , payloadPassphrase :: Passphrase "addr-derivation-payload"
    -- ^ Used for encryption of payload containing address derivation path.
    } deriving stock (Generic)

instance (NFData key, NFData (DerivationPathFrom depth)) => NFData (ByronKey depth key)
deriving instance (Show key, Show (DerivationPathFrom depth)) => Show (ByronKey depth key)
deriving instance (Eq key, Eq (DerivationPathFrom depth)) => Eq (ByronKey depth key)

-- | The hierarchical derivation indices for a given level/depth.
type family DerivationPathFrom (depth :: Depth) :: Type where
    -- The root key is generated from the seed.
    DerivationPathFrom 'RootK =
        ()
    -- The account key is generated from the root key and account index.
    DerivationPathFrom 'AccountK =
        Index 'WholeDomain 'AccountK
    -- The address key is generated from the account key and address index.
    DerivationPathFrom 'AddressK =
        (Index 'WholeDomain 'AccountK, Index 'WholeDomain 'AddressK)

instance WalletKey ByronKey where
    changePassphrase = changePassphraseRnd
    -- Extract the public key part of a private key.
    publicKey = mapKey toXPub
    -- Hash a public key to some other representation.
    digest = hash . unXPub . getKey
    getRawKey = getKey
    liftRawKey = error "not supported"
    keyTypeDescriptor _ = "rnd"

instance KnownNat pm => PaymentAddress ('Testnet pm) ByronKey where
    paymentAddress k = Address
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getKey k)
            [ CBOR.encodeDerivationPathAttr pwd acctIx addrIx
            , CBOR.encodeProtocolMagicAttr (testnetMagic @pm)
            ]
      where
        (acctIx, addrIx) = derivationPath k
        pwd = payloadPassphrase k
    liftPaymentAddress (KeyFingerprint bytes) =
        Address bytes

instance PaymentAddress 'Mainnet ByronKey where
    paymentAddress k = Address
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getKey k)
            [ CBOR.encodeDerivationPathAttr pwd acctIx addrIx ]
      where
        (acctIx, addrIx) = derivationPath k
        pwd = payloadPassphrase k
    liftPaymentAddress (KeyFingerprint bytes) =
        Address bytes

instance MkKeyFingerprint ByronKey Address where
    paymentKeyFingerprint addr@(Address bytes) =
        case CBOR.deserialiseCbor CBOR.decodeAddressPayload bytes of
            Just _  -> Right $ KeyFingerprint bytes
            Nothing -> Left $ ErrInvalidAddress addr (Proxy @ByronKey)

{-------------------------------------------------------------------------------
                                 Key generation
-------------------------------------------------------------------------------}

-- | The amount of entropy carried by a BIP-39 12-word mnemonic is 16 bytes.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- | Generate a root key from a corresponding seed.
-- The seed should be at least 16 bytes.
generateKeyFromSeed
    :: SomeMnemonic
    -> Passphrase "encryption"
    -> ByronKey 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed ()

-- | Generate a new key from seed. Note that the @depth@ is left open so that
-- the caller gets to decide what type of key this is. This is mostly for
-- testing, in practice, seeds are used to represent root keys, and one should
-- use 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: DerivationPathFrom depth
    -> SomeMnemonic
    -> Passphrase "encryption"
    -> ByronKey depth XPrv
unsafeGenerateKeyFromSeed derivationPath (SomeMnemonic mw) (Passphrase pwd) = ByronKey
    { getKey = masterKey
    , derivationPath
    , payloadPassphrase = hdPassphrase (toXPub masterKey)
    }
  where
    masterKey = generate (hashSeed seed') pwd
    seed  = entropyToBytes $ mnemonicToEntropy mw
    seed' = invariant
        ("seed length : " <> show (BA.length seed)
            <> " in (Passphrase \"seed\") is not valid")
        seed
        (\s -> BA.length s >= minSeedLengthBytes && BA.length s <= 255)

-- | Hash the seed entropy (generated from mnemonic) used to initiate a HD
-- wallet. This increases the key length to 34 bytes, selectKey is greater than the
-- minimum for 'generate' (32 bytes).
--
-- Note that our current implementation deviates from BIP-39 because we use a
-- hash function (Blake2b) rather than key stretching with PBKDF2.
--
-- There are two methods of hashing the seed entropy, for different use cases.
--
-- 1. Normal random derivation wallet seeds. The seed entropy is hashed using
--    Blake2b_256, inside a double CBOR serialization sandwich.
--
-- 2. Seeds for redeeming paper wallets. The seed entropy is hashed using
--    Blake2b_256, without any serialization.
hashSeed :: ScrubbedBytes -> ScrubbedBytes
hashSeed = BA.convert . cbor . blake2b256 . cbor . BA.convert
  where
    cbor = CBOR.toStrictByteString . CBOR.encodeBytes

-- | Derive a symmetric key for encrypting and authenticating the address
-- derivation path. PBKDF2 encryption using HMAC with the hash algorithm SHA512
-- is employed.
hdPassphrase :: XPub -> Passphrase "addr-derivation-payload"
hdPassphrase masterKey = Passphrase $
    PBKDF2.generate
    (PBKDF2.prfHMAC SHA512)
    (PBKDF2.Parameters 500 32)
    (unXPub masterKey)
    ("address-hashing" :: ByteString)

mkByronKeyFromMasterKey
    :: XPrv
    -> ByronKey 'RootK XPrv
mkByronKeyFromMasterKey = unsafeMkByronKeyFromMasterKey ()

unsafeMkByronKeyFromMasterKey
    :: DerivationPathFrom depth
    -> XPrv
    -> ByronKey depth XPrv
unsafeMkByronKeyFromMasterKey derivationPath masterKey = ByronKey
    { getKey = masterKey
    , derivationPath
    , payloadPassphrase = hdPassphrase (toXPub masterKey)
    }

{-------------------------------------------------------------------------------
                                   Passphrase
-------------------------------------------------------------------------------}

-- | Re-encrypt the private key using a different passphrase, and regenerate
-- the payload passphrase.
--
-- **Important**:
-- This function doesn't check that the old passphrase is correct! Caller is
-- expected to have already checked that. Using an incorrect passphrase here
-- will lead to very bad thing.
changePassphraseRnd
    :: (PassphraseScheme, Passphrase "user")
    -> (PassphraseScheme, Passphrase "user")
    -> ByronKey depth XPrv
    -> ByronKey depth XPrv
changePassphraseRnd old new key = ByronKey
    { getKey = masterKey
    , derivationPath = derivationPath key
    , payloadPassphrase = hdPassphrase (toXPub masterKey)
    }
  where
    masterKey = changePassphraseXPrv old new (getKey key)

{-------------------------------------------------------------------------------
                                 HD derivation
-------------------------------------------------------------------------------}

-- TODO
-- This instance is unsound. It only exists because we need to derive the
-- reward account in the wallet engine when making transaction (in case there
-- are any withdrawals).
--
-- With 'ByronKey', withdrawals will always be `0`, and the result of this
-- function shouldn't be evaluated (relying on laziness here). If they do, then
-- we're doing something wrong.
instance W.HardDerivation ByronKey where
    type AddressIndexDerivationType ByronKey = 'WholeDomain

    deriveAccountPrivateKey _ _ _ = error
        "unsound evaluation of 'deriveAccountPrivateKey' in the context of Byron key"

    deriveAddressPrivateKey _ _ _ _ = error
        "unsound evaluation of 'deriveAddressPrivateKey' in the context of Byron key"

-- | Derives account private key from the given root private key, using
-- derivation scheme 1.
--
-- NOTE: The caller is expected to provide the corresponding passphrase (and to
-- have checked that the passphrase is valid). Providing a wrong passphrase will
-- not make the function fail but will instead, yield an incorrect new key that
-- doesn't belong to the wallet.
deriveAccountPrivateKey
    :: Passphrase "encryption"
    -> ByronKey 'RootK XPrv
    -> Index 'WholeDomain 'AccountK
    -> ByronKey 'AccountK XPrv
deriveAccountPrivateKey (Passphrase pwd) masterKey idx@(Index accIx) = ByronKey
    { getKey = deriveXPrv DerivationScheme1 pwd (getKey masterKey) accIx
    , derivationPath = idx
    , payloadPassphrase = payloadPassphrase masterKey
    }

-- | Derives address private key from the given account private key, using
-- derivation scheme 1.
--
-- NOTE: The caller is expected to provide the corresponding passphrase (and to
-- have checked that the passphrase is valid). Providing a wrong passphrase will
-- not make the function fail but will instead, yield an incorrect new key that
-- doesn't belong to the wallet.
deriveAddressPrivateKey
    :: Passphrase "encryption"
    -> ByronKey 'AccountK XPrv
    -> Index 'WholeDomain 'AddressK
    -> ByronKey 'AddressK XPrv
deriveAddressPrivateKey (Passphrase pwd) accountKey idx@(Index addrIx) = ByronKey
    { getKey = deriveXPrv DerivationScheme1 pwd (getKey accountKey) addrIx
    , derivationPath = (derivationPath accountKey, idx)
    , payloadPassphrase = payloadPassphrase accountKey
    }

{-------------------------------------------------------------------------------
                          Storing and retrieving keys
-------------------------------------------------------------------------------}

instance PersistPrivateKey (ByronKey 'RootK) where
    serializeXPrv ((ByronKey k _ (Passphrase p)), h) =
        ( hex (unXPrv k) <> ":" <> hex p
        , hex . getPassphraseHash $ h
        )

    unsafeDeserializeXPrv (k, h) = either err id $ (,)
        <$> fmap mkKey (deserializeKey k)
        <*> fmap PassphraseHash (fromHex h)
      where
        err _ = error "unsafeDeserializeXPrv: unable to deserialize ByronKey"
        mkKey (key, pwd) = ByronKey key () pwd
        deserializeKey
            :: ByteString
            -> Either String
                ( XPrv
                , Passphrase "addr-derivation-payload"
                )
        deserializeKey b = case map (fromHex @ByteString) (B8.split ':' b) of
            [Right rawK, Right p] ->
                case xprv rawK of
                    Right k' -> Right (k', Passphrase (BA.convert p))
                    Left e -> Left e
            _ ->
                Left "Key input must be two hex strings separated by :"

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

-- | Transform the wrapped key.
mapKey :: (key -> key') -> ByronKey depth key -> ByronKey depth key'
mapKey f rnd = rnd { getKey = f (getKey rnd) }
