{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2019 IOHK
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

    -- * Primitive Crypto Types
    , XPub (..)
    , ChainCode (..)
    , XPrv
    , unXPub
    , unXPrv
    , xprv
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
    , dummyAddress

    -- * Passphrase
    , Passphrase(..)
    , PassphraseMinLength(..)
    , PassphraseMaxLength(..)
    , FromMnemonic(..)
    , FromMnemonicError(..)
    , ErrWrongPassphrase(..)
    , encryptPassphrase
    , checkPassphrase
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( ChainCode (..), XPrv, XPub (..), unXPrv, unXPub, xprv )
import Cardano.Wallet.Primitive.Mnemonic
    ( CheckSumBits
    , ConsistentEntropy
    , DictionaryError (..)
    , EntropyError (..)
    , EntropySize
    , MnemonicError (..)
    , MnemonicWordsError (..)
    , entropyToBytes
    , mkMnemonic
    , mnemonicToEntropy
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Hash (..) )
import Control.Arrow
    ( left )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( unless )
import Crypto.Hash
    ( Digest, HashAlgorithm )
import Crypto.KDF.PBKDF2
    ( Parameters (..), fastPBKDF2_SHA512 )
import Crypto.Random.Types
    ( MonadRandom (..) )
import Data.Bifunctor
    ( first )
import Data.ByteArray
    ( ByteArray, ByteArrayAccess, ScrubbedBytes )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.List
    ( intercalate )
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

import qualified Basement.Compat.Base as B
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

{-------------------------------------------------------------------------------
                                 Passphrases
-------------------------------------------------------------------------------}

-- | An encapsulated passphrase. The inner format is free, but the wrapper helps
-- readability in function signatures.
newtype Passphrase (purpose :: Symbol) = Passphrase ScrubbedBytes
    deriving stock (Eq, Show)
    deriving newtype (Semigroup, Monoid, NFData)

type role Passphrase phantom

class PassphraseMinLength (purpose :: Symbol) where
    -- | Minimal Length for a passphrase, for lack of better validations
    passphraseMinLength :: Proxy purpose -> Int

class PassphraseMaxLength (purpose :: Symbol) where
    -- | Maximum length for a passphrase
    passphraseMaxLength :: Proxy purpose -> Int

instance PassphraseMinLength "encryption" where passphraseMinLength _ = 10
instance PassphraseMaxLength "encryption" where passphraseMaxLength _ = 255
instance PassphraseMinLength "encryption-old" where
    passphraseMinLength _ = passphraseMinLength (Proxy @"encryption")
instance PassphraseMaxLength "encryption-old" where
    passphraseMaxLength _ = passphraseMaxLength (Proxy @"encryption")
instance PassphraseMinLength "encryption-new" where
    passphraseMinLength _ = passphraseMinLength (Proxy @"encryption")
instance PassphraseMaxLength "encryption-new" where
    passphraseMaxLength _ = passphraseMaxLength (Proxy @"encryption")

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

-- | Create a passphrase from a mnemonic sentence. This class enables caller to
-- parse text list of variable length into mnemonic sentences.
--
-- >>> fromMnemonic @'[12,15,18,21] @"generation" ["toilet", "curse", ... ]
-- Right (Passphrase <ScrubbedBytes>)
--
-- Note that the given 'Nat's **have** to be valid mnemonic sizes, otherwise the
-- underlying code won't even compile, with not-so-friendly error messages.
class FromMnemonic (sz :: [Nat]) (purpose :: Symbol) where
    fromMnemonic :: [Text] -> Either (FromMnemonicError sz) (Passphrase purpose)

-- | Error reported from trying to create a passphrase from a given mnemonic
newtype FromMnemonicError (sz :: [Nat]) =
    FromMnemonicError { getFromMnemonicError :: String }
    deriving stock (Eq, Show)
    deriving newtype Buildable

instance {-# OVERLAPS #-}
    ( n ~ EntropySize mw
    , csz ~ CheckSumBits n
    , ConsistentEntropy n mw csz
    , FromMnemonic rest purpose
    , NatVals rest
    ) =>
    FromMnemonic (mw ': rest) purpose
  where
    fromMnemonic parts = case parseMW of
        Left err -> left (promote err) parseRest
        Right mw -> Right mw
      where
        parseMW = left (FromMnemonicError . getFromMnemonicError) $ -- coerce
            fromMnemonic @'[mw] @purpose parts
        parseRest = left (FromMnemonicError . getFromMnemonicError) $ -- coerce
            fromMnemonic @rest @purpose parts
        promote e e' =
            let
                sz = fromEnum <$> natVals (Proxy :: Proxy (mw ': rest))
                mw = fromEnum $ natVal (Proxy :: Proxy mw)
            in if length parts `notElem` sz
                then FromMnemonicError
                    $  "Invalid number of words: "
                    <> intercalate ", " (show <$> init sz)
                    <> (if length sz > 1 then " or " else "") <> show (last sz)
                    <> " words are expected."
                else if length parts == mw then e else e'

-- | Small helper to collect 'Nat' values from a type-level list
class NatVals (ns :: [Nat]) where
    natVals :: Proxy ns -> [Integer]

instance NatVals '[] where
    natVals _ = []

instance (KnownNat n, NatVals rest) => NatVals (n ': rest) where
    natVals _ = natVal (Proxy :: Proxy n) : natVals (Proxy :: Proxy rest)

instance
    ( n ~ EntropySize mw
    , csz ~ CheckSumBits n
    , ConsistentEntropy n mw csz
    ) =>
    FromMnemonic (mw ': '[]) purpose
  where
    fromMnemonic parts = do
        m <- first (FromMnemonicError . pretty) (mkMnemonic @mw parts)
        return $ Passphrase $ entropyToBytes $ mnemonicToEntropy m
      where
        pretty = \case
            ErrMnemonicWords ErrWrongNumberOfWords{} ->
                "Invalid number of words: "
                <> show (natVal (Proxy :: Proxy mw))
                <> " words are expected."
            ErrDictionary (ErrInvalidDictionaryWord w) ->
                "Found invalid (non-English) word: \"" <> B.toList w <> "\"."
            ErrEntropy ErrInvalidEntropyChecksum{} ->
                "Invalid entropy checksum: please double-check the last word of \
                \your mnemonic sentence."
            ErrEntropy ErrInvalidEntropyLength{} ->
                "Something went wrong when trying to generate the entropy from \
                \the given mnemonic. As a user, there's nothing you can do."

-- | Encrypt a 'Passphrase' into a format that is suitable for storing on disk
encryptPassphrase
    :: MonadRandom m => Passphrase purpose -> m (Hash purpose)
encryptPassphrase (Passphrase bytes) = do
    salt <- getRandomBytes @_ @ByteString 16
    let params = Parameters
            { iterCounts = 20000
            , outputLength = 64
            }
    return $ Hash $ BA.convert $ mempty
        <> BS.singleton (fromIntegral (BS.length salt))
        <> salt
        <> BA.convert @ByteString (fastPBKDF2_SHA512 params bytes salt)

-- | Check whether a 'Passphrase' matches with a stored 'Hash'
checkPassphrase
    :: Passphrase purpose
    -> Hash purpose
    -> Either ErrWrongPassphrase ()
checkPassphrase received stored = do
    salt <- getSalt stored
    unless (constantTimeEq (encryptPassphrase received salt) stored) $
        Left ErrWrongPassphrase
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
data NetworkDiscriminant = Mainnet | Testnet
    deriving (Generic, Show, Eq, Bounded, Enum)

instance NFData NetworkDiscriminant

instance FromText NetworkDiscriminant where
    fromText = fromTextToBoundedEnum SnakeLowerCase

instance ToText NetworkDiscriminant where
    toText = toTextFromBoundedEnum SnakeLowerCase

class NetworkDiscriminantVal (n :: NetworkDiscriminant) where
    networkDiscriminantVal :: NetworkDiscriminant

instance NetworkDiscriminantVal 'Mainnet where networkDiscriminantVal = Mainnet
instance NetworkDiscriminantVal 'Testnet where networkDiscriminantVal = Testnet

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
        :: Passphrase "encryption-old"
        -> Passphrase "encryption-new"
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

    -- | Produce a fake address key of this scheme, for use in 'dummyAddress'.
    dummyKey :: key 'AddressK XPub

-- | Encoding of addresses for certain key types and backend targets.
class MkKeyFingerprint key
    => PaymentAddress (network :: NetworkDiscriminant) key where
    -- | Convert a public key to a payment 'Address' valid for the given
    -- network discrimination.
    --
    -- Note that 'paymentAddress' is ambiguous and requires therefore a type
    -- application.
    paymentAddress
        :: key 'AddressK XPub
        -> Address

    -- | Lift a payment fingerprint back into an address.
    liftPaymentFingerprint
        :: KeyFingerprint "payment" key
        -> Address

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
            -- ^ Staking key
        -> Address

-- | Produce a fake address of representative size for the target and key
-- type. This can be used in transaction size estimations.
--
-- This function is ambiguous, like 'paymentAddress', and types need to be
-- applied.
dummyAddress
    :: forall network key.
        ( PaymentAddress network key
        , WalletKey key
        )
    => Address
dummyAddress =
    paymentAddress @network @key (dummyKey @key)

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
class MkKeyFingerprint (key :: Depth -> * -> *) where
    paymentKeyFingerprint
        :: Address
        -> Either
            (ErrMkKeyFingerprint key)
            (KeyFingerprint "payment" key)

    delegationKeyFingerprint
        :: Address
        -> Either
            (ErrMkKeyFingerprint key)
            (Maybe (KeyFingerprint "delegation" key))

data ErrMkKeyFingerprint key
    = ErrInvalidAddress Address (Proxy key) deriving (Show, Eq)

{-------------------------------------------------------------------------------
                                Helpers
-------------------------------------------------------------------------------}

-- | Encode a 'ByteString' in base16
hex :: ByteArrayAccess bin => bin -> ByteString
hex = convertToBase Base16

-- | Decode a 'ByteString' from base16
fromHex :: ByteArray bout => ByteString -> Either String bout
fromHex = convertFromBase Base16
