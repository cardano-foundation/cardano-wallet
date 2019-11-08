{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
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
    ( -- * Types
      ShelleyKey(..)

    -- * Constants
    , minSeedLengthBytes
    , publicKeySize
    , addrSingleSize
    , addrGroupedSize

    -- * Generation and derivation
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed

    -- * Address
    , decodeShelleyAddress
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( DerivationScheme (..)
    , XPrv
    , XPub (..)
    , deriveXPrv
    , deriveXPub
    , generateNew
    , toXPub
    , unXPrv
    , unXPub
    , xPrvChangePass
    , xprv
    , xpub
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , Depth (..)
    , DerivationType (..)
    , ErrMkKeyFingerprint (..)
    , HardDerivation (..)
    , Index (..)
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , NetworkDiscriminantVal
    , Passphrase (..)
    , PaymentAddress (..)
    , PersistPrivateKey (..)
    , PersistPublicKey (..)
    , SoftDerivation (..)
    , WalletKey (..)
    , fromHex
    , hex
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
import Data.ByteString
    ( ByteString )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( TextDecodingError (..) )
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

instance HardDerivation ShelleyKey where
    type AddressIndexDerivationType ShelleyKey = 'Soft

    deriveAccountPrivateKey
            (Passphrase pwd) (ShelleyKey rootXPrv) (Index accIx) =
        let
            purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
                deriveXPrv DerivationScheme2 pwd rootXPrv purposeIndex
            coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
                deriveXPrv DerivationScheme2 pwd purposeXPrv coinTypeIndex
            acctXPrv = -- lvl3 derivation; hardened derivation of account' index
                deriveXPrv DerivationScheme2 pwd coinTypeXPrv accIx
        in
            ShelleyKey acctXPrv

    deriveAddressPrivateKey
            (Passphrase pwd) (ShelleyKey accXPrv) accountingStyle (Index addrIx) =
        let
            changeCode =
                fromIntegral $ fromEnum accountingStyle
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 pwd accXPrv changeCode
            addrXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 pwd changeXPrv addrIx
        in
            ShelleyKey addrXPrv

instance SoftDerivation ShelleyKey where
    deriveAddressPublicKey (ShelleyKey accXPub) accountingStyle (Index addrIx) =
        fromMaybe errWrongIndex $ do
            let changeCode = fromIntegral $ fromEnum accountingStyle
            changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
                deriveXPub DerivationScheme2 accXPub changeCode
            addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
                deriveXPub DerivationScheme2 changeXPub addrIx
            return $ ShelleyKey addrXPub
      where
        errWrongIndex = error $
            "deriveAddressPublicKey failed: was given an hardened (or too big) \
            \index for soft path derivation ( " ++ show addrIx ++ "). This is \
            \either a programmer error, or, we may have reached the maximum \
            \number of addresses for a given wallet."

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
    paymentAddress (ShelleyKey k0) =
        encodeShelleyAddress (single @'Mainnet) [xpubPublicKey k0]
    liftPaymentFingerprint (KeyFingerprint k0) =
        encodeShelleyAddress (single @'Mainnet) [k0]

instance PaymentAddress 'Testnet ShelleyKey where
    paymentAddress (ShelleyKey k0) =
        encodeShelleyAddress (single @'Testnet) [xpubPublicKey k0]
    liftPaymentFingerprint (KeyFingerprint k0) =
        encodeShelleyAddress (single @'Testnet) [k0]

instance DelegationAddress 'Mainnet ShelleyKey where
    delegationAddress (ShelleyKey k0) (ShelleyKey k1) =
        encodeShelleyAddress (grouped @'Mainnet) (xpubPublicKey <$> [k0, k1])

instance DelegationAddress 'Testnet ShelleyKey where
    delegationAddress (ShelleyKey k0) (ShelleyKey k1) =
        encodeShelleyAddress (grouped @'Testnet) (xpubPublicKey <$> [k0, k1])

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

-- | Internal function to encode shelley addresses from key fingerprints.
--
-- We use this to define both instance separately to avoid having to carry a
-- 'KnownNetwork' constraint around.
encodeShelleyAddress :: Word8 -> [ByteString] -> Address
encodeShelleyAddress discriminant keys =
    Address $ invariantSize $ BL.toStrict $ runPut $ do
        putWord8 discriminant
        mapM_ putByteString keys
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
        n -> Left $ invalidAddressLength n
    return (Address bytes)
  where
    invalidAddressLength actualLength = TextDecodingError $
        "Invalid address length ("
        <> show actualLength
        <> "): expected either "
        <> show addrSingleSize
        <> " or "
        <> show addrGroupedSize
        <> " bytes."
    invalidFirstByte = TextDecodingError
        "Invalid address first byte."
    invalidNetwork = TextDecodingError $
        "This address belongs to another network. Network is: "
        <> show (networkDiscriminantVal @n) <> "."

instance MkKeyFingerprint ShelleyKey where
    paymentKeyFingerprint addr@(Address bytes)
        | len == addrSingleSize || len == addrGroupedSize =
            Right $ KeyFingerprint $ BS.take publicKeySize $ BS.drop 1 bytes
        | otherwise =
            Left $ ErrInvalidAddress addr (Proxy @ShelleyKey)
      where
        len = BS.length bytes

    delegationKeyFingerprint addr@(Address bytes)
        | len == addrSingleSize =
            Right Nothing
        | len == addrGroupedSize =
            Right $ Just $ KeyFingerprint $ BS.drop addrSingleSize bytes
        | otherwise =
            Left $ ErrInvalidAddress addr (Proxy @ShelleyKey)
      where
        len = BS.length bytes

{-------------------------------------------------------------------------------
                          Storing and retrieving keys
-------------------------------------------------------------------------------}

instance PersistPrivateKey (ShelleyKey 'RootK) where
    serializeXPrv (k, h) =
        ( hex . unXPrv . getKey $ k
        , hex . getHash $ h
        )

    unsafeDeserializeXPrv (k, h) = either err id $ (,)
        <$> fmap ShelleyKey (xprvFromText k)
        <*> fmap Hash (fromHex h)
      where
        xprvFromText = xprv <=< fromHex @ByteString
        err _ = error "unsafeDeserializeXPrv: unable to deserialize ShelleyKey"

instance PersistPublicKey (ShelleyKey 'AccountK) where
    serializeXPub =
        hex . unXPub . getKey

    unsafeDeserializeXPub =
        either err ShelleyKey . xpubFromText
      where
        xpubFromText = xpub <=< fromHex @ByteString
        err _ = error "unsafeDeserializeXPub: unable to deserialize ShelleyKey"
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
