{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
    , Role (..)
    , utxoExternal
    , utxoInternal
    , mutableAccount
    , zeroAccount
    , stakeDerivationPath
    , DerivationType (..)
    , HardDerivation (..)
    , SoftDerivation (..)
    , DerivationPrefix (..)
    , DerivationIndex (..)
    , liftIndex
    , hashVerificationKey

    -- * Delegation
    , RewardAccount (..)
    , ToRewardAccount(..)
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
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, xpubPublicKey )
import Cardano.Address.Script
    ( KeyHash (..), KeyRole )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..), PassphraseHash (..), PassphraseScheme )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( (>=>) )
import Crypto.Hash
    ( Digest, HashAlgorithm )
import Crypto.Hash.Utils
    ( blake2b224 )
import Data.ByteArray
    ( ByteArray, ByteArrayAccess )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Kind
    ( Type )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Scientific
    ( Scientific, toBoundedInteger )
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
import Quiet
    ( Quiet (..) )
import Safe
    ( readMay, toEnumMay )

import qualified Data.Text as T

{-------------------------------------------------------------------------------
                                HD Hierarchy
-------------------------------------------------------------------------------}

-- | Typically used as a phantom type parameter, a witness to the type of the
-- key being used.
--
-- For example, @key 'RootK XPrv@, represents the private key at the root of the
-- HD hierarchy.
--
-- According to BIP-0044 / CIP-1852, we have the following keys in our HD
-- hierarchy:
--
-- @m | purpose' | cointype' | account' | role | address@
--
-- Plus, we also have script keys (which are used in shared wallets) and policy
-- keys (which are used in minting and burning).
data Depth
    = RootK
    | PurposeK
    | CoinTypeK
    | AccountK
    | RoleK
    | AddressK
    | ScriptK
    | PolicyK

-- | Marker for addresses type engaged. We want to handle four cases here.
-- The first two are pertinent to UTxO accounting,
-- next handles rewards from participation in staking
-- the last one is used for getting verification keys used in scripts.
-- (a) external chain is used for addresses that are part of the 'advertised'
--     targets of a given transaction
-- (b) internal change is for addresses used to handle the change of a
--     the transaction within a given wallet
-- (c) the addresses for a reward account
-- (d) used for keys used inside scripts
data Role
    = UtxoExternal
    | UtxoInternal
    | MutableAccount
    deriving (Generic, Typeable, Show, Eq, Ord, Bounded)

instance NFData Role

-- Not deriving 'Enum' because this could have a dramatic impact if we were
-- to assign the wrong index to the corresponding constructor (by swapping
-- around the constructor above for instance).
instance Enum Role where
    toEnum = \case
        0 -> UtxoExternal
        1 -> UtxoInternal
        2 -> MutableAccount
        _ -> error "Role.toEnum: bad argument"
    fromEnum = \case
        UtxoExternal -> 0
        UtxoInternal -> 1
        MutableAccount -> 2

instance ToText Role where
    toText = toTextFromBoundedEnum SnakeLowerCase

instance FromText Role where
    fromText = fromTextToBoundedEnum SnakeLowerCase

-- | smart-constructor for getting a derivation index that refers to external
-- utxo.
utxoExternal :: Index 'Soft 'RoleK
utxoExternal = toEnum $ fromEnum UtxoExternal

-- | smart-constructor for getting a derivation index that refers to internal
-- utxo.
utxoInternal :: Index 'Soft 'RoleK
utxoInternal = toEnum $ fromEnum UtxoInternal

-- | smart-constructor for getting a derivation index that refers to stake
-- key level (a.k.a mutable account)
mutableAccount :: Index 'Soft 'RoleK
mutableAccount = toEnum $ fromEnum MutableAccount

zeroAccount :: Index 'Soft 'AddressK
zeroAccount = minBound

-- | Full path to the stake key. There's only one.
stakeDerivationPath :: DerivationPrefix -> NonEmpty DerivationIndex
stakeDerivationPath (DerivationPrefix (purpose, coin, acc)) =
    (fromIndex purpose) :| [
      fromIndex coin
    , fromIndex acc
    , fromIndex mutableAccount
    , fromIndex zeroAccount]
  where
    fromIndex :: Index t l -> DerivationIndex
    fromIndex = DerivationIndex . getIndex

-- | A thin wrapper around derivation indexes. This can be used to represent
-- derivation path as homogeneous lists of 'DerivationIndex'. This is slightly
-- more convenient than having to carry heterogeneous lists of 'Index depth type'
-- and works fine because:
--
-- 1. The 'depth' matters not because what the depth captures is actually the
--    position of the index in that list. It makes sense to carry at the type
--    level when manipulating standalone indexes to avoid mistakes, but when
--    treating them as a part of a list it is redundant.
--
-- 2. The derivationType is captured by representing indexes as plain Word32.
--    The Soft / Hardened notation is for easing human-readability but in the
--    end, a soft index is simply a value < 2^31, whereas a "hardened" index is
--    simply a value >= 2^31. Therefore, instead of representing indexes as
--    derivationType + relative index within 0 and 2^31, we can represent them
--    as just an index between 0 and 2^32, which is what DerivationIndex does.
newtype DerivationIndex
    = DerivationIndex { getDerivationIndex :: Word32 }
    deriving (Eq, Ord, Generic)
    deriving Show via (Quiet DerivationIndex)

instance NFData DerivationIndex

instance ToText DerivationIndex where
    toText (DerivationIndex ix)
        | ix >= firstHardened  = T.pack $ show (ix - firstHardened) <> "H"
        | otherwise = T.pack $ show ix
      where
        firstHardened = getIndex @'Hardened minBound

instance FromText DerivationIndex where
    fromText source =
        if "H" `T.isSuffixOf` source then do
            DerivationIndex ix <- castNumber (T.init source) >>= parseAsScientific
            pure $ DerivationIndex $ ix + firstHardened
        else
            castNumber source >>= parseAsScientific
      where
        firstHardened = getIndex @'Hardened minBound

        errMalformed = TextDecodingError $ unwords
            [ "A derivation index must be a natural number between"
            , show (getIndex @'Soft minBound)
            , "and"
            , show (getIndex @'Soft maxBound)
            , "with an optional 'H' suffix (e.g. '1815H' or '44')."
            , "Indexes without suffixes are called 'Soft'"
            , "Indexes with suffixes are called 'Hardened'."
            ]

        parseAsScientific :: Scientific -> Either TextDecodingError DerivationIndex
        parseAsScientific x =
            case toBoundedInteger x of
                Just ix | ix < firstHardened ->
                    pure $ DerivationIndex ix
                _ ->
                    Left errMalformed

        castNumber :: Text -> Either TextDecodingError Scientific
        castNumber txt =
            case readMay (T.unpack txt) of
                Nothing ->
                    Left errMalformed
                Just s ->
                    pure s

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

-- | Each 'SeqState' is like a bucket of addresses associated with an 'account'.
-- An 'account' corresponds to a subset of an HD tree as defined in BIP-0039.
--
-- cardano-wallet implements two similar HD schemes on top of BIP-0039 that are:
--
-- - BIP-0044 (for so-called Icarus wallets)
-- - CIP-1815 (for so-called Shelley and Jormungandr wallets)
--
-- Both scheme works by considering 5 levels of derivation from an initial root
-- key (see also 'Depth' from Cardano.Wallet.Primitive.AddressDerivation). A
-- SeqState keeps track of indexes from the two last levels of a derivation
-- branch. The 'DerivationPrefix' defines the first three indexes chosen for
-- this particular 'SeqState'.
newtype DerivationPrefix = DerivationPrefix
    ( Index 'Hardened 'PurposeK
    , Index 'Hardened 'CoinTypeK
    , Index 'Hardened 'AccountK
    ) deriving (Show, Generic, Eq, Ord)

instance NFData DerivationPrefix

instance ToText DerivationPrefix where
    toText (DerivationPrefix (purpose, coinType, account))
        = T.intercalate "/"
        $ map toText
        [getIndex purpose, getIndex coinType, getIndex account]

instance FromText DerivationPrefix where
    fromText txt =
        DerivationPrefix <$> case T.splitOn "/" txt of
            [purposeT, coinTypeT, accountT] -> (,,)
                <$> fromText purposeT
                <*> fromText coinTypeT
                <*> fromText accountT
            _ ->
                Left $ TextDecodingError "expected exactly 3 derivation paths"

-- | Type of derivation that should be used with the given indexes.
--
-- In theory, we should only consider two derivation types: soft and hard.
--
-- However, historically, addresses in Cardano used to be generated across the
-- both soft and hard domain. We therefore introduce a 'WholeDomain' derivation
-- type that is the exact union of `Hardened` and `Soft`.
data DerivationType = Hardened | Soft | WholeDomain

-- | An interface for doing hard derivations from the root private key
class HardDerivation (key :: Depth -> Type -> Type) where
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
        -> Role
        -> Index (AddressIndexDerivationType key) 'AddressK
        -> key 'AddressK XPrv

-- | An interface for doing soft derivations from an account public key
class HardDerivation key => SoftDerivation (key :: Depth -> Type -> Type) where
    -- | Derives address public key from the given account public key, using
    -- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
    -- package for more details).
    --
    -- This is the preferred way of deriving new sequential address public keys.
    deriveAddressPublicKey
        :: key 'AccountK XPub
        -> Role
        -> Index 'Soft 'AddressK
        -> key 'AddressK XPub

-- | Derivation of a reward account, as a type-class because different between
-- key types (in particular, Jörmungandr vs Shelley).
class ToRewardAccount k where
    toRewardAccount :: k 'AddressK XPub -> RewardAccount
    someRewardAccount :: SomeMnemonic -> (XPrv, RewardAccount, NonEmpty DerivationIndex)

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

hashVerificationKey
    :: WalletKey key
    => KeyRole
    -> key depth XPub
    -> KeyHash
hashVerificationKey keyRole =
    KeyHash keyRole . blake2b224 . xpubPublicKey . getRawKey

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

class WalletKey (key :: Depth -> Type -> Type) where
    -- | Re-encrypt a private key using a different passphrase.
    --
    -- **Important**:
    -- This function doesn't check that the old passphrase is correct! Caller is
    -- expected to have already checked that. Using an incorrect passphrase here
    -- will lead to very bad thing.
    changePassphrase
        :: (PassphraseScheme, Passphrase "user")
            -- ^ Old passphrase
        -> (PassphraseScheme, Passphrase "user")
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

    -- | Lift 'XPrv' or 'XPub' to 'WalletKey'.
    liftRawKey
        :: raw
        -> key depth raw

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
class PersistPrivateKey (key :: Type -> Type) where
    -- | Convert a private key and its password hash into hexadecimal strings
    -- suitable for storing in a text file or database column.
    serializeXPrv
        :: (key XPrv, PassphraseHash)
        -> (ByteString, ByteString)

    -- | The reverse of 'serializeXPrv'. This may fail if the inputs are not
    -- valid hexadecimal strings, or if the key is of the wrong length.
    unsafeDeserializeXPrv
        :: (ByteString, ByteString)
        -> (key XPrv, PassphraseHash)

-- | Operations for saving a public key into a database, and restoring it from
-- a database. The keys should be encoded in hexadecimal strings.
class PersistPublicKey (key :: Type -> Type) where
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
class Show from => MkKeyFingerprint (key :: Depth -> Type -> Type) from where
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
