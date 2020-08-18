{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Primitives for performing address derivation for some given schemes. This is
-- where most of the crypto happens in the wallet and, it is quite important to
-- ensure that the implementations match with other Cardano wallets
-- (like cardano-sl, Yoroi/Icarus, or cardano-cli)
--
-- The actual implementations are in the following modules:
--
--  * "Cardano.Wallet.Primitive.AddressDerivation.Shelley"
--  * "Cardano.Wallet.Primitive.AddressDerivation.Byron"

module Cardano.Wallet.Primitive.AddressDerivation
    (
    -- * HD Derivation
      Depth (..)
    , Index (..)
    , AccountingStyle (..)
    , DerivationType (..)
    , HardDerivation (..)
    , SoftDerivation (..)
    , liftIndex

    -- * Delegation
    , ChimericAccount (..)
    , ToChimericAccount(..)
    , deriveRewardAccount

    -- * Helpers
    , hex
    , fromHex

    -- * Network Discrimination
    , NetworkDiscriminant (..)
    , NetworkDiscriminantVal
    , networkDiscriminantVal

    -- * Backends Interoperability
    , PaymentAddress(..)
    , DelegationAddress(..)
    , WalletKey(..)
    , PersistPrivateKey(..)
    , PersistPublicKey(..)
    , MkKeyFingerprint(..)
    , ErrMkKeyFingerprint(..)
    , KeyFingerprint(..)

    -- * Passphrase
    , Passphrase(..)
    , PassphraseMinLength(..)
    , PassphraseMaxLength(..)
    , ErrWrongPassphrase(..)
    , PassphraseScheme(..)
    , encryptPassphrase
    , checkPassphrase
    , preparePassphrase
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Cardano.Wallet.Primitive.Types
    ( Address (..), ChimericAccount (..), Hash (..), PassphraseScheme (..) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( unless, (>=>) )
import Crypto.Hash
    ( Digest, HashAlgorithm )
import Crypto.Hash.Utils
    ( blake2b256 )
import Crypto.KDF.PBKDF2
    ( Parameters (..), fastPBKDF2_SHA512 )
import Crypto.Random.Types
    ( MonadRandom (..) )
import Data.ByteArray
    ( ByteArray, ByteArrayAccess, ScrubbedBytes )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Proxy
    ( Proxy (..) )
import Data.String
    ( fromString )
import Data.Text
    ( Text )
import Data.Text.Class
    ( CaseStyle (..)
    , FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    , fromTextToBoundedEnum
    , toTextFromBoundedEnum
    )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownNat, Nat, Symbol, natVal )
import Safe
    ( toEnumMay )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.Scrypt as Scrypt
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                                HD Hierarchy
-------------------------------------------------------------------------------}

-- | Key Depth in the derivation path, according to BIP-0039 / BIP-0044
--
-- @m | purpose' | cointype' | account' | change | address@
--
-- We do not manipulate purpose, cointype and change paths directly, so they are
-- left out of the sum type.
data Depth = RootK | AccountK | AddressK

-- | Marker for addresses type engaged. We want to handle three cases here.
-- The first two are pertinent to UTxO accounting
-- and the last one handles rewards from participation in staking.
-- (a) external chain is used for addresses that are part of the 'advertised'
--     targets of a given transaction
-- (b) internal change is for addresses used to handle the change of a
--     the transaction within a given wallet
-- (c) the addresses for a reward (chimeric) account
data AccountingStyle
    = UTxOExternal
    | UTxOInternal
    | MutableAccount
    deriving (Generic, Typeable, Show, Eq, Ord, Bounded)

instance NFData AccountingStyle

-- Not deriving 'Enum' because this could have a dramatic impact if we were
-- to assign the wrong index to the corresponding constructor (by swapping
-- around the constructor above for instance).
instance Enum AccountingStyle where
    toEnum = \case
        0 -> UTxOExternal
        1 -> UTxOInternal
        2 -> MutableAccount
        _ -> error "AccountingStyle.toEnum: bad argument"
    fromEnum = \case
        UTxOExternal -> 0
        UTxOInternal -> 1
        MutableAccount -> 2

instance ToText AccountingStyle where
    toText = toTextFromBoundedEnum SnakeLowerCase

instance FromText AccountingStyle where
    fromText = fromTextToBoundedEnum SnakeLowerCase

-- | A derivation index, with phantom-types to disambiguate derivation type.
--
-- @
-- let accountIx = Index 'Hardened 'AccountK
-- let addressIx = Index 'Soft 'AddressK
-- @
newtype Index (derivationType :: DerivationType) (level :: Depth) = Index
    { getIndex :: Word32 }
    deriving stock (Generic, Show, Eq, Ord)

instance NFData (Index derivationType level)

instance Bounded (Index 'Hardened level) where
    minBound = Index 0x80000000
    maxBound = Index maxBound

instance Bounded (Index 'Soft level) where
    minBound = Index minBound
    maxBound = let (Index ix) = minBound @(Index 'Hardened _) in Index (ix - 1)

instance Bounded (Index 'WholeDomain level) where
    minBound = Index minBound
    maxBound = Index maxBound

instance Enum (Index 'Hardened level) where
    fromEnum (Index ix) = fromIntegral ix
    toEnum ix
        | Index (fromIntegral ix) < minBound @(Index 'Hardened _) =
            error "Index@Hardened.toEnum: bad argument"
        | otherwise =
            Index (fromIntegral ix)

instance Enum (Index 'Soft level) where
    fromEnum (Index ix) = fromIntegral ix
    toEnum ix
        | Index (fromIntegral ix) > maxBound @(Index 'Soft _) =
            error "Index@Soft.toEnum: bad argument"
        | otherwise =
            Index (fromIntegral ix)

instance Enum (Index 'WholeDomain level) where
    fromEnum (Index ix) = fromIntegral ix
    toEnum ix
        | Index (fromIntegral ix) > maxBound @(Index 'WholeDomain _) =
            error "Index@WholeDomain.toEnum: bad argument"
        | otherwise =
            Index (fromIntegral ix)

instance Buildable (Index derivationType level) where
    build (Index ix) = fromString (show ix)

instance
  ( Enum (Index derivation level)
  , Bounded (Index derivation level)
  ) => FromText (Index derivation level) where
    fromText = fromText >=> \n -> case toEnumMay n of
        Just ix -> pure ix
        Nothing -> Left $ TextDecodingError $ unwords
            [ "Couldn't parse derivation index. Expected an integer between"
            , show (minBound @(Index derivation level))
            , "and"
            , show (maxBound @(Index derivation level))
            ]

-- Safe coercion to WholeDomain from smaller domains.
class LiftIndex derivation where
    liftIndex :: Index derivation level -> Index 'WholeDomain level

instance LiftIndex 'Hardened where
    liftIndex (Index ix) = Index ix

instance LiftIndex 'Soft where
    liftIndex (Index ix) = Index ix

-- | Type of derivation that should be used with the given indexes.
--
-- In theory, we should only consider two derivation types: soft and hard.
--
-- However, historically, addresses in Cardano used to be generated across the
-- both soft and hard domain. We therefore introduce a 'WholeDomain' derivation
-- type that is the exact union of `Hardened` and `Soft`.
data DerivationType = Hardened | Soft | WholeDomain

-- | An interface for doing hard derivations from the root private key
class HardDerivation (key :: Depth -> * -> *) where
    type AddressIndexDerivationType key :: DerivationType

    -- | Derives account private key from the given root private key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    --
    -- NOTE: The caller is expected to provide the corresponding passphrase (and
    -- to have checked that the passphrase is valid). Providing a wrong passphrase
    -- will not make the function fail but will instead, yield an incorrect new
    -- key that doesn't belong to the wallet.
    deriveAccountPrivateKey
        :: Passphrase "encryption"
        -> key 'RootK XPrv
        -> Index 'Hardened 'AccountK
        -> key 'AccountK XPrv

    -- | Derives address private key from the given account private key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    --
    -- It is preferred to use 'deriveAddressPublicKey' whenever possible to avoid
    -- having to manipulate passphrases and private keys.
    --
    -- NOTE: The caller is expected to provide the corresponding passphrase (and
    -- to have checked that the passphrase is valid). Providing a wrong passphrase
    -- will not make the function fail but will instead, yield an incorrect new
    -- key that doesn't belong to the wallet.
    deriveAddressPrivateKey
        :: Passphrase "encryption"
        -> key 'AccountK XPrv
        -> AccountingStyle
        -> Index (AddressIndexDerivationType key) 'AddressK
        -> key 'AddressK XPrv

-- | An interface for doing soft derivations from an account public key
class HardDerivation key => SoftDerivation (key :: Depth -> * -> *) where
    -- | Derives address public key from the given account public key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    --
    -- This is the preferred way of deriving new sequential address public keys.
    deriveAddressPublicKey
        :: key 'AccountK XPub
        -> AccountingStyle
        -> Index 'Soft 'AddressK
        -> key 'AddressK XPub

-- | Derivation of a reward account, as a type-class because different between
-- key types (in particular, Jörmungandr vs Shelley).
class ToChimericAccount k where
    toChimericAccount :: k 'AddressK XPub -> ChimericAccount
    someChimericAccount :: SomeMnemonic -> (XPrv, ChimericAccount)

-- | Derive a reward account from a root private key. It is agreed by standard
-- that every HD wallet will use only a single reward account. This account is
-- located into a special derivation path and uses the first index of that path.
deriveRewardAccount
    :: ( HardDerivation k
       , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
       )
    => Passphrase "encryption"
    -> k 'RootK XPrv
    -> k 'AddressK XPrv
deriveRewardAccount pwd rootPrv =
    let accPrv = deriveAccountPrivateKey pwd rootPrv minBound
    in deriveAddressPrivateKey pwd accPrv MutableAccount minBound

{-------------------------------------------------------------------------------
                                 Passphrases
-------------------------------------------------------------------------------}

-- | An encapsulated passphrase. The inner format is free, but the wrapper helps
-- readability in function signatures.
newtype Passphrase (purpose :: Symbol) = Passphrase ScrubbedBytes
    deriving stock (Eq, Show)
    deriving newtype (Semigroup, Monoid, NFData, ByteArrayAccess)

type role Passphrase phantom

class PassphraseMinLength (purpose :: Symbol) where
    -- | Minimal Length for a passphrase, for lack of better validations
    passphraseMinLength :: Proxy purpose -> Int

class PassphraseMaxLength (purpose :: Symbol) where
    -- | Maximum length for a passphrase
    passphraseMaxLength :: Proxy purpose -> Int

instance PassphraseMinLength "raw" where passphraseMinLength _ = 10
instance PassphraseMaxLength "raw" where passphraseMaxLength _ = 255

instance PassphraseMinLength "lenient" where passphraseMinLength _ = 0
instance PassphraseMaxLength "lenient" where passphraseMaxLength _ = 255

instance
    ( PassphraseMaxLength purpose
    , PassphraseMinLength purpose
    ) => FromText (Passphrase purpose) where
    fromText t
        | T.length t < minLength =
            Left $ TextDecodingError $
                "passphrase is too short: expected at least "
                <> show minLength <> " characters"
        | T.length t > maxLength =
            Left $ TextDecodingError $
                "passphrase is too long: expected at most "
                <> show maxLength <> " characters"
        | otherwise =
            pure $ Passphrase $ BA.convert $ T.encodeUtf8 t
      where
        minLength = passphraseMinLength (Proxy :: Proxy purpose)
        maxLength = passphraseMaxLength (Proxy :: Proxy purpose)

instance ToText (Passphrase purpose) where
    toText (Passphrase bytes) = T.decodeUtf8 $ BA.convert bytes

-- | Encrypt a 'Passphrase' into a format that is suitable for storing on disk
encryptPassphrase
    :: MonadRandom m
    => Passphrase "encryption"
    -> m (Hash "encryption")
encryptPassphrase  (Passphrase bytes) = do
    salt <- getRandomBytes @_ @ByteString 16
    let params = Parameters
            { iterCounts = 20000
            , outputLength = 64
            }
    return $ Hash $ BA.convert $ mempty
        <> BS.singleton (fromIntegral (BS.length salt))
        <> salt
        <> BA.convert @ByteString (fastPBKDF2_SHA512 params bytes salt)

-- | Manipulation done on legacy passphrases before getting encrypted.
preparePassphrase
    :: PassphraseScheme
    -> Passphrase "raw"
    -> Passphrase "encryption"
preparePassphrase = \case
    EncryptWithPBKDF2 -> coerce
    EncryptWithScrypt -> Passphrase . hashMaybe
  where
    hashMaybe pw@(Passphrase bytes)
        | pw == mempty = BA.convert bytes
        | otherwise = BA.convert $ blake2b256 bytes

-- | Check whether a 'Passphrase' matches with a stored 'Hash'
checkPassphrase
    :: PassphraseScheme
    -> Passphrase "raw"
    -> Hash "encryption"
    -> Either ErrWrongPassphrase ()
checkPassphrase scheme received stored = do
    let prepared = preparePassphrase scheme received
    case scheme of
        EncryptWithPBKDF2 -> do
            salt <- getSalt stored
            unless (constantTimeEq (encryptPassphrase prepared salt) stored) $
                Left ErrWrongPassphrase
        EncryptWithScrypt -> do
            let msg = Scrypt.Pass
                    $ CBOR.toStrictByteString
                    $ CBOR.encodeBytes
                    $ BA.convert prepared
            if Scrypt.verifyPass' msg (Scrypt.EncryptedPass (getHash stored))
                then Right ()
                else Left ErrWrongPassphrase
  where
    getSalt :: Hash purpose -> Either ErrWrongPassphrase (Passphrase "salt")
    getSalt (Hash bytes) = do
        len <- case BS.unpack (BS.take 1 bytes) of
            [len] -> Right $ fromIntegral len
            _ -> Left ErrWrongPassphrase
        Right $ Passphrase $ BA.convert $ BS.take len (BS.drop 1 bytes)
    constantTimeEq :: Hash purpose -> Hash purpose -> Bool
    constantTimeEq (Hash a) (Hash b) =
        BA.convert @_ @ScrubbedBytes a == BA.convert @_ @ScrubbedBytes b

-- | Indicate a failure when checking for a given 'Passphrase' match
data ErrWrongPassphrase = ErrWrongPassphrase
    deriving stock (Show, Eq)

-- | Little trick to be able to provide our own "random" salt in order to
-- deterministically re-compute a passphrase hash from a known salt. Note that,
-- this boils down to giving an extra argument to the `encryptPassphrase`
-- function which is the salt, in order to make it behave deterministically.
--
-- @
-- encryptPassphrase
--   :: MonadRandom m
--   => Passphrase purpose
--   -> m (Hash purpose)
--
--  ~
--
-- encryptPassphrase
--   :: Passphrase purpose
--   -> Passphrase "salt"
--   -> m (Hash purpose)
-- @
--
-- >>> encryptPassphrase pwd (Passphrase @"salt" salt)
-- Hash "..."
--
instance MonadRandom ((->) (Passphrase "salt")) where
    getRandomBytes _ (Passphrase salt) = BA.convert salt

{-------------------------------------------------------------------------------
                             Network Discrimination
-------------------------------------------------------------------------------}

-- | Available network options.
--
-- - @Mainnet@: is a shortcut for quickly pointing to mainnet. On Byron and
--              Shelley, it assumes no discrimination. It has a known magic and
--              known genesis parameters.
--
-- - @Testnet@: can be used to identify any network that has a custom genesis
--              and, that requires _explicit_ network discrimination in
--              addresses. Genesis file needs to be passed explicitly when
--              starting the application.
--
-- - @Staging@: very much like testnet, but like mainnet, assumes to no address
--              discrimination. Genesis file needs to be passed explicitly when
--              starting the application.
--
data NetworkDiscriminant = Mainnet | Testnet Nat | Staging Nat
    deriving Typeable

class NetworkDiscriminantVal (n :: NetworkDiscriminant) where
    networkDiscriminantVal :: Text

instance NetworkDiscriminantVal 'Mainnet where
    networkDiscriminantVal =
        "mainnet"

instance KnownNat pm => NetworkDiscriminantVal ('Testnet pm) where
    networkDiscriminantVal =
        "testnet (" <> T.pack (show $ natVal $ Proxy @pm) <> ")"

instance KnownNat pm => NetworkDiscriminantVal ('Staging pm) where
    networkDiscriminantVal =
        "staging (" <> T.pack (show $ natVal $ Proxy @pm) <> ")"

{-------------------------------------------------------------------------------
                     Interface over keys / address types
-------------------------------------------------------------------------------}

class WalletKey (key :: Depth -> * -> *) where
    -- | Re-encrypt a private key using a different passphrase.
    --
    -- **Important**:
    -- This function doesn't check that the old passphrase is correct! Caller is
    -- expected to have already checked that. Using an incorrect passphrase here
    -- will lead to very bad thing.
    changePassphrase
        :: Passphrase "encryption"
            -- ^ Old passphrase
        -> Passphrase "encryption"
            -- ^ New passphrase
        -> key depth XPrv
        -> key depth XPrv

    -- | Extract the public key part of a private key.
    publicKey
        :: key depth XPrv
        -> key depth XPub

    -- | Hash a public key to some other representation.
    digest
        :: HashAlgorithm a
        => key depth XPub
        -> Digest a

    -- | Get a short, human-readable string descriptor that uniquely identifies
    --   the specified key type.
    keyTypeDescriptor :: Proxy key -> String

    -- | Unwrap the 'WalletKey' to use the 'XPrv' or 'XPub'.
    getRawKey
        :: key depth raw
        -> raw

-- | Encoding of addresses for certain key types and backend targets.
class MkKeyFingerprint key Address
    => PaymentAddress (network :: NetworkDiscriminant) key where
    -- | Convert a public key to a payment 'Address' valid for the given
    -- network discrimination.
    --
    -- Note that 'paymentAddress' is ambiguous and requires therefore a type
    -- application.
    paymentAddress
        :: key 'AddressK XPub
        -> Address

    -- | Lift a payment fingerprint back into a payment address.
    liftPaymentAddress
        :: KeyFingerprint "payment" key
            -- ^ Payment fingerprint
        -> Address

instance PaymentAddress 'Mainnet k => PaymentAddress ('Staging pm) k where
    paymentAddress = paymentAddress @'Mainnet
    liftPaymentAddress = liftPaymentAddress @'Mainnet

class PaymentAddress network key
    => DelegationAddress (network :: NetworkDiscriminant) key where
    -- | Convert a public key and a staking key to a delegation 'Address' valid
    -- for the given network discrimination. Funds sent to this address will be
    -- delegated according to the delegation settings attached to the delegation
    -- key.
    --
    -- Note that 'delegationAddress' is ambiguous and requires therefore a type
    -- application.
    delegationAddress
        :: key 'AddressK XPub
            -- ^ Payment key
        -> key 'AddressK XPub
            -- ^ Staking key / Reward account
        -> Address

    -- | Lift a payment fingerprint back into a delegation address.
    liftDelegationAddress
        :: KeyFingerprint "payment" key
            -- ^ Payment fingerprint
        -> key 'AddressK XPub
            -- ^ Staking key / Reward account
        -> Address

instance DelegationAddress 'Mainnet k => DelegationAddress ('Staging pm) k where
    delegationAddress = delegationAddress @'Mainnet
    liftDelegationAddress = liftDelegationAddress @'Mainnet

-- | Operations for saving a private key into a database, and restoring it from
-- a database. The keys should be encoded in hexadecimal strings.
class PersistPrivateKey (key :: * -> *) where
    -- | Convert a private key and its password hash into hexadecimal strings
    -- suitable for storing in a text file or database column.
    serializeXPrv
        :: (key XPrv, Hash "encryption")
        -> (ByteString, ByteString)

    -- | The reverse of 'serializeXPrv'. This may fail if the inputs are not
    -- valid hexadecimal strings, or if the key is of the wrong length.
    unsafeDeserializeXPrv
        :: (ByteString, ByteString)
        -> (key XPrv, Hash "encryption")

-- | Operations for saving a public key into a database, and restoring it from
-- a database. The keys should be encoded in hexadecimal strings.
class PersistPublicKey (key :: * -> *) where
    -- | Convert a private key and its password hash into hexadecimal strings
    -- suitable for storing in a text file or database column.
    serializeXPub
        :: key XPub
        -> ByteString

    -- | Convert a public key into hexadecimal strings suitable for storing in
    -- a text file or database column.
    unsafeDeserializeXPub
        :: ByteString
        -> key XPub

-- | Something that uniquely identifies a public key. Typically,
-- a hash of that key or the key itself.
newtype KeyFingerprint (s :: Symbol) key = KeyFingerprint ByteString
    deriving (Generic, Show, Eq, Ord)

instance NFData (KeyFingerprint s key)

-- | Produce 'KeyFingerprint' for existing types. A fingerprint here uniquely
-- identifies part of an address. It can refer to either the payment key or, if
-- any, the delegation key of an address.
--
-- The fingerprint obeys the following rules:
--
-- - If two addresses are the same, then they have the same fingerprints
-- - It is possible to lift the fingerprint back into an address
--
-- This second rule pretty much fixes what can be chosen as a fingerprint for
-- various key types:
--
-- 1. For 'ByronKey', it can only be the address itself!
-- 2. For 'ShelleyKey', then the "payment" fingerprint refers to the payment key
--    within a single or grouped address.
class Show from => MkKeyFingerprint (key :: Depth -> * -> *) from where
    paymentKeyFingerprint
        :: from
        -> Either
            (ErrMkKeyFingerprint key from)
            (KeyFingerprint "payment" key)

data ErrMkKeyFingerprint key from
    = ErrInvalidAddress from (Proxy key) deriving (Show, Eq)

{-------------------------------------------------------------------------------
                                Helpers
-------------------------------------------------------------------------------}

-- | Encode a 'ByteString' in base16
hex :: ByteArrayAccess bin => bin -> ByteString
hex = convertToBase Base16

-- | Decode a 'ByteString' from base16
fromHex :: ByteArray bout => ByteString -> Either String bout
fromHex = convertFromBase Base16
