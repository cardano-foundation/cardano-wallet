{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- Implementation of address derivation for the sequential schemes, as
-- implemented by Yoroi/Icarus and cardano-cli.

module Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( -- * ShelleyKey types
      ShelleyKey(..)
    , ChangeChain(..)
    -- * ShelleyKey generation and derivation
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed
    , minSeedLengthBytes
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deriveAddressPublicKey
    -- * Passphrase
    , changePassphraseSeq
    -- * Storing and retrieving keys
    , deserializeXPubSeq
    , serializeXPubSeq
    -- * Encoding / Decoding
    , decodeShelleyAddress
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( DerivationScheme (..)
    , XPrv
    , XPub
    , deriveXPrv
    , deriveXPub
    , generateNew
    , toXPub
    , unXPrv
    , unXPub
    , xPrvChangePass
    , xprv
    , xpub
    , xpubPublicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , Depth (..)
    , DerivationType (..)
    , Index (..)
    , InspectAddress (..)
    , NetworkDiscriminant (..)
    , NetworkDiscriminantVal
    , Passphrase (..)
    , PaymentAddress (..)
    , PersistKey (..)
    , WalletKey (..)
    , networkDiscriminantVal
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Hash (..), invariant )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( when, (<=<) )
import Crypto.Hash
    ( Digest, HashAlgorithm, hash )
import Data.Binary.Put
    ( putByteString, putWord8, runPut )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
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
    ( Word32, Word8 )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

{-------------------------------------------------------------------------------
                            Sequential Derivation
-------------------------------------------------------------------------------}

-- | A cryptographic key for sequential-scheme address derivation, with
-- phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = ShelleyKey 'RootK XPrv
-- let accountPubKey = ShelleyKey 'AccountK XPub
-- let addressPubKey = ShelleyKey 'AddressK XPub
-- @
newtype ShelleyKey (depth :: Depth) key =
    ShelleyKey { getKey :: key }
    deriving stock (Generic, Show, Eq)

instance (NFData key) => NFData (ShelleyKey depth key)

-- | Marker for the change chain. In practice, change of a transaction goes onto
-- the addresses generated on the internal chain, whereas the external chain is
-- used for addresses that are part of the 'advertised' targets of a transaction
data ChangeChain
    = ExternalChain
    | InternalChain
    deriving (Generic, Typeable, Show, Eq, Ord, Bounded)

instance NFData ChangeChain

-- Not deriving 'Enum' because this could have a dramatic impact if we were
-- to assign the wrong index to the corresponding constructor (by swapping
-- around the constructor above for instance).
instance Enum ChangeChain where
    toEnum = \case
        0 -> ExternalChain
        1 -> InternalChain
        _ -> error "ChangeChain.toEnum: bad argument"
    fromEnum = \case
        ExternalChain -> 0
        InternalChain -> 1

instance ToText ChangeChain where
    toText = toTextFromBoundedEnum SnakeLowerCase

instance FromText ChangeChain where
    fromText = fromTextToBoundedEnum SnakeLowerCase

-- | Size, in bytes, of a public key (without chain code)
publicKeySize :: Int
publicKeySize = 32

-- Serialized length in bytes of a Single Address
addrSingleSize :: Int
addrSingleSize = 1 + publicKeySize

-- Serialized length in bytes of a Grouped Address
addrGroupedSize :: Int
addrGroupedSize = addrSingleSize + publicKeySize

-- | Purpose is a constant set to 44' (or 0x8000002C) following the BIP-44
-- recommendation. It indicates that the subtree of this node is used
-- according to this specification.
--
-- Hardened derivation is used at this level.
purposeIndex :: Word32
purposeIndex = 0x8000002C

-- | One master node (seed) can be used for unlimited number of independent
-- cryptocoins such as Bitcoin, Litecoin or Namecoin. However, sharing the
-- same space for various cryptocoins has some disadvantages.
--
-- This level creates a separate subtree for every cryptocoin, avoiding reusing
-- addresses across cryptocoins and improving privacy issues.
--
-- Coin type is a constant, set for each cryptocoin. For Cardano this constant
-- is set to 1815' (or 0x80000717). 1815 is the birthyear of our beloved Ada
-- Lovelace.
--
-- Hardened derivation is used at this level.
coinTypeIndex :: Word32
coinTypeIndex = 0x80000717

-- | The minimum seed length for 'generateKeyFromSeed' and
-- 'unsafeGenerateKeyFromSeed'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- | Generate a root key from a corresponding seed.
-- The seed should be at least 16 bytes.
generateKeyFromSeed
    :: (Passphrase "seed", Passphrase "generation")
       -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> ShelleyKey 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed

-- | Generate a new key from seed. Note that the @depth@ is left open so that
-- the caller gets to decide what type of key this is. This is mostly for
-- testing, in practice, seeds are used to represent root keys, and one should
-- use 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: (Passphrase "seed", Passphrase "generation")
        -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> ShelleyKey depth XPrv
unsafeGenerateKeyFromSeed (Passphrase seed, Passphrase gen) (Passphrase pwd) =
    let
        seed' = invariant
            ("seed length : " <> show (BA.length seed) <> " in (Passphrase \"seed\") is not valid")
            seed
            (\s -> BA.length s >= minSeedLengthBytes && BA.length s <= 255)
    in ShelleyKey $ generateNew seed' gen pwd

-- | Derives account private key from the given root private key, using
-- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
-- package for more details).
--
-- NOTE: The caller is expected to provide the corresponding passphrase (and to
-- have checked that the passphrase is valid). Providing a wrong passphrase will
-- not make the function fail but will instead, yield an incorrect new key that
-- doesn't belong to the wallet.
deriveAccountPrivateKey
    :: Passphrase "encryption"
    -> ShelleyKey 'RootK XPrv
    -> Index 'Hardened 'AccountK
    -> ShelleyKey 'AccountK XPrv
deriveAccountPrivateKey (Passphrase pwd) (ShelleyKey rootXPrv) (Index accIx) =
    let
        purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
            deriveXPrv DerivationScheme2 pwd rootXPrv purposeIndex
        coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
            deriveXPrv DerivationScheme2 pwd purposeXPrv coinTypeIndex
        acctXPrv = -- lvl3 derivation; hardened derivation of account' index
            deriveXPrv DerivationScheme2 pwd coinTypeXPrv accIx
    in
        ShelleyKey acctXPrv

-- | Derives address private key from the given account private key, using
-- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
-- package for more details).
--
-- It is preferred to use 'deriveAddressPublicKey' whenever possible to avoid
-- having to manipulate passphrases and private keys.
--
-- NOTE: The caller is expected to provide the corresponding passphrase (and to
-- have checked that the passphrase is valid). Providing a wrong passphrase will
-- not make the function fail but will instead, yield an incorrect new key that
-- doesn't belong to the wallet.
deriveAddressPrivateKey
    :: Passphrase "encryption"
    -> ShelleyKey 'AccountK XPrv
    -> ChangeChain
    -> Index 'Soft 'AddressK
    -> ShelleyKey 'AddressK XPrv
deriveAddressPrivateKey
        (Passphrase pwd) (ShelleyKey accXPrv) changeChain (Index addrIx) =
    let
        changeCode =
            fromIntegral $ fromEnum changeChain
        changeXPrv = -- lvl4 derivation; soft derivation of change chain
            deriveXPrv DerivationScheme2 pwd accXPrv changeCode
        addrXPrv = -- lvl5 derivation; soft derivation of address index
            deriveXPrv DerivationScheme2 pwd changeXPrv addrIx
    in
        ShelleyKey addrXPrv

-- | Derives address public key from the given account public key, using
-- derivation scheme 2 (see <https://github.com/input-output-hk/cardano-crypto/ cardano-crypto>
-- package for more details).
--
-- This is the preferred way of deriving new sequential address public keys.
deriveAddressPublicKey
    :: ShelleyKey 'AccountK XPub
    -> ChangeChain
    -> Index 'Soft 'AddressK
    -> ShelleyKey 'AddressK XPub
deriveAddressPublicKey (ShelleyKey accXPub) changeChain (Index addrIx) =
    fromMaybe errWrongIndex $ do
        let changeCode = fromIntegral $ fromEnum changeChain
        changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
            deriveXPub DerivationScheme2 accXPub changeCode
        addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
            deriveXPub DerivationScheme2 changeXPub addrIx
        return $ ShelleyKey addrXPub
  where
    errWrongIndex = error $
        "Cardano.Wallet.Primitive.AddressDerivation.deriveAddressPublicKey \
        \failed: was given an hardened (or too big) index for soft path \
        \derivation ( " ++ show addrIx ++ "). This is either a programmer \
        \error, or, we may have reached the maximum number of addresses for \
        \a given wallet."

{-------------------------------------------------------------------------------
                            WalletKey implementation
-------------------------------------------------------------------------------}

instance WalletKey ShelleyKey where
    changePassphrase = changePassphraseSeq
    publicKey = publicKeySeq
    digest = digestSeq
    getRawKey = getKey
    dummyKey = dummyKeySeq
    keyTypeDescriptor _ = "seq"

-- | Extract the public key part of a private key.
publicKeySeq
    :: ShelleyKey depth XPrv
    -> ShelleyKey depth XPub
publicKeySeq (ShelleyKey k) =
    ShelleyKey (toXPub k)

-- | Hash a public key to some other representation.
digestSeq
    :: HashAlgorithm a
    => ShelleyKey depth XPub
    -> Digest a
digestSeq (ShelleyKey k) =
    hash (unXPub k)

dummyKeySeq :: ShelleyKey 'AddressK XPub
dummyKeySeq = ShelleyKey key
    where Right key = xpub (BS.replicate 64 0)

-- | Re-encrypt a private key using a different passphrase.
--
-- **Important**:
-- This function doesn't check that the old passphrase is correct! Caller is
-- expected to have already checked that. Using an incorrect passphrase here
-- will lead to very bad thing.
changePassphraseSeq
    :: Passphrase "encryption-old"
    -> Passphrase "encryption-new"
    -> ShelleyKey depth XPrv
    -> ShelleyKey depth XPrv
changePassphraseSeq (Passphrase oldPwd) (Passphrase newPwd) (ShelleyKey prv) =
    ShelleyKey $ xPrvChangePass oldPwd newPwd prv

{-------------------------------------------------------------------------------
                         Relationship Key / Address
-------------------------------------------------------------------------------}

instance PaymentAddress 'Mainnet ShelleyKey where
    paymentAddress xpub0 =
        encodeShelleyAddress (single @'Mainnet) [getKey xpub0]

instance PaymentAddress 'Testnet ShelleyKey where
    paymentAddress xpub0 =
        encodeShelleyAddress (single @'Testnet) [getKey xpub0]

instance DelegationAddress 'Mainnet ShelleyKey where
    delegationAddress xpub0 xpub1 =
        encodeShelleyAddress (grouped @'Mainnet) [getKey xpub0, getKey xpub1]

instance DelegationAddress 'Testnet ShelleyKey where
    delegationAddress xpub0 xpub1 =
        encodeShelleyAddress (grouped @'Testnet) [getKey xpub0, getKey xpub1]

-- | Embed some constants into a network type.
class KnownNetwork (n :: NetworkDiscriminant) where
    single :: Word8
        -- ^ Address discriminant byte for single addresses, this is the first byte of
        -- every addresses using the Shelley format carrying only a spending key.
    grouped :: Word8
        -- ^ Address discriminant byte for grouped addresses, this is the first byte of
        -- every addresses using the Shelley format carrying both a spending and a
        -- delegation key.

instance KnownNetwork 'Mainnet where
    single = 0x03
    grouped = 0x04

instance KnownNetwork 'Testnet where
    single = 0x83
    grouped = 0x84

-- | We use this to define both instance separately to avoid having to carry a
-- 'KnownNetwork' constraint around.
encodeShelleyAddress :: Word8 -> [XPub] -> Address
encodeShelleyAddress discriminant keys =
    Address $ invariantSize $ BL.toStrict $ runPut $ do
        putWord8 discriminant
        mapM_ putByteString (xpubPublicKey <$> keys)
  where
    invariantSize :: HasCallStack => ByteString -> ByteString
    invariantSize bytes
        | BS.length bytes == len = bytes
        | otherwise = error
            $ "length was "
            ++ show (BS.length bytes)
            ++ ", but expected to be "
            ++ (show len)
      where
        len = 1 + length keys * publicKeySize

-- | Verify the structure of a payload decoded from a Bech32 text string
decodeShelleyAddress
    :: forall n. (KnownNetwork n, NetworkDiscriminantVal n)
    => ByteString
    -> Either TextDecodingError Address
decodeShelleyAddress bytes = do
    case BS.length bytes of
        n | n == addrSingleSize -> do
            let firstByte = BS.take 1 bytes
            when (firstByte /= BS.pack [single @n]) $
                if firstByte `elem`
                    (BS.pack <$> [[single @'Mainnet], [single @'Testnet]])
                    then Left invalidNetwork
                    else Left invalidFirstByte
        n | n == addrGroupedSize -> do
            let firstByte = BS.take 1 bytes
            when (firstByte /= BS.pack [grouped @n]) $
                if firstByte `elem`
                    (BS.pack <$> [[grouped @'Mainnet], [grouped @'Testnet]])
                    then Left invalidNetwork
                    else Left invalidFirstByte
        _ ->
            Left $ TextDecodingError $
                "Invalid Address length (" <> show (BS.length bytes)
                <> "): expected either "
                <> show addrSingleSize
                <> " or "
                <> show addrGroupedSize
                <> " bytes."
    return (Address bytes)
  where
    invalidFirstByte = TextDecodingError
        "Invalid Address first byte."
    invalidNetwork = TextDecodingError $
        "This Address belongs to another network. Network is: "
        <> show (networkDiscriminantVal @n) <> "."

-- FIXME
-- 'ShelleyKey' (as well as 'ByronKey') was actually a wrong division for separating
-- HD derivations schemes.
-- Keys are actually the same entities, but the division operates at the address
-- level (Byron addresses have a different structure than Shelley ones).
-- This class here makes strong assumptions on the structure of the address and
-- addresses are the in the new Shelley format -- which is an okay-ish
-- assumption since we've been treating 'ShelleyKey' as key associated with Shelley
-- addresses so far. But this will change as soon as we decide to support Yoroi
-- legacy wallets.
instance InspectAddress ShelleyKey where
    type SpendingKey ShelleyKey = ByteString
    getSpendingKey (Address bytes)
        | let l = BS.length bytes in l == addrSingleSize || l == addrGroupedSize =
            BS.take publicKeySize $ BS.drop 1 bytes
        | otherwise =
            error "InspectAddress: tried to inspect an incompatible address"

    type DelegationKey ShelleyKey = ByteString
    getDelegationKey (Address bytes)
        | BS.length bytes == addrSingleSize =
            Nothing
        | BS.length bytes == addrGroupedSize =
            Just $ BS.drop addrSingleSize bytes
        | otherwise =
            error "InspectAddress: tried to inspect an incompatible address"

{-------------------------------------------------------------------------------
                          Storing and retrieving keys
-------------------------------------------------------------------------------}

instance PersistKey ShelleyKey where
    serializeXPrv = serializeXPrvSeq
    deserializeXPrv = deserializeXPrvSeq

-- | Convert a private key and its password hash into hexadecimal strings
-- suitable for storing in a text file or database column.
serializeXPrvSeq
    :: (ShelleyKey 'RootK XPrv, Hash "encryption")
    -> (ByteString, ByteString)
serializeXPrvSeq (k, h) =
    ( toHexText . unXPrv . getRawKey $ k
    , toHexText . getHash $ h )

-- | The reverse of 'serializeXPrvSeq'. This may fail if the inputs are not
-- valid hexadecimal strings, or if the key is of the wrong length.
deserializeXPrvSeq
    :: (ByteString, ByteString)
       -- ^ Hexadecimal encoded private key and password hash
    -> Either String (ShelleyKey purpose XPrv, Hash "encryption")
deserializeXPrvSeq (k, h) = (,)
    <$> fmap ShelleyKey (xprvFromText k)
    <*> fmap Hash (fromHexText h)
  where
    xprvFromText = xprv <=< fromHexText

-- | Convert a public key into a hexadecimal string suitable for storing in a
-- text file or database column.
serializeXPubSeq :: ShelleyKey purpose XPub -> ByteString
serializeXPubSeq = toHexText . unXPub . getRawKey

-- | The reverse of 'serializeXPub'. This will fail if the input is not
-- hexadecimal string of the correct length.
deserializeXPubSeq :: ByteString -> Either String (ShelleyKey purpose XPub)
deserializeXPubSeq = fmap ShelleyKey . (xpub <=< fromHexText)

toHexText :: ByteString -> ByteString
toHexText = convertToBase Base16

fromHexText :: ByteString -> Either String ByteString
fromHexText = convertFromBase Base16

-- $use
-- 'Key' and 'Index' allow for representing public keys, private keys, hardened
-- indexes and soft (non-hardened) indexes for various level in a non-ambiguous
-- manner. Those types are opaque and can only be created through dedicated
-- constructors.
--
-- Indexes can be created through their `Bounded` and `Enum` instances. Note
-- that, the `Enum` functions are partial and its the caller responsibility to
-- make sure it is safe to call them. For instance, calling @succ maxBound@ for
-- any index would through a runtime error. Similarly, trying to convert an
-- invalid value from an 'Int' to a an 'Index' will result in bad behavior.
--
-- >>> toEnum 0x00000000 :: Index 'Soft 'AddressK
-- Index {getIndex = 0}
--
-- >>> toEnum 0x00000000 :: Index 'Hardened 'AccountK
-- Index {getIndex = *** Exception: Index@Hardened.toEnum: bad argument
--
-- >>> toEnum 0x80000000 :: Index 'Hardened 'AccountK
-- Index {getIndex = 2147483648}
--
-- >>> toEnum 0x80000000 :: Index 'Soft 'AddressK
-- Index {getIndex = *** Exception: Index@Soft.toEnum: bad argument
--
-- Keys have to be created from a seed using 'generateKeyFromSeed' which always
-- gives a root private key. Then, child keys can be created safely using the
-- various key derivation methods:
--
-- - 'publicKey' --> For any @ShelleyKey _ XPrv@ to @ShelleyKey _ XPub@
-- - 'deriveAccountPrivateKey' -->
--      From @ShelleyKey RootK XPrv@ to @ShelleyKey AccountK XPrv@
-- - 'deriveAddressPrivateKey' -->
--      From @ShelleyKey AccountK XPrv@ to @ShelleyKey AddressK XPrv@
-- - 'deriveAddressPublicKey' -->
--      From @ShelleyKey AccountK XPub@ to @ShelleyKey AddressK XPub@
--
-- For example:
--
-- @
-- -- Access public key for: m|coinType'|purpose'|0'
-- let seed = "My Secret Seed"
--
-- let rootXPrv :: ShelleyKey 'RootK XPrv
--     rootXPrv = generateKeyFromSeed (seed, mempty) mempty
--
-- let accIx :: Index 'Hardened 'AccountK
--     accIx = toEnum 0x80000000
--
-- let accXPub :: ShelleyKey 'AccountL XPub
--     accXPub = publicKey $ deriveAccountPrivateKey mempty rootXPrv accIx
-- @
