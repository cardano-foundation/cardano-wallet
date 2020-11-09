{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Implementation of address derivation for 'Jormungandr' Keys.

module Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( -- * Types
      JormungandrKey(..)

    -- * Constants
    , minSeedLengthBytes
    , publicKeySize
    , addrSingleSize
    , addrGroupedSize
    , KnownNetwork (..)

    -- * Generation and derivation
    , generateKeyFromSeed
    , unsafeGenerateKeyFromSeed
    , xpubFromText

    -- * Address
    , decodeJormungandrAddress
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
import Cardano.Mnemonic
    ( SomeMnemonic (..), entropyToBytes, mnemonicToEntropy )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChimericAccount (..)
    , DelegationAddress (..)
    , Depth (..)
    , DerivationIndex (..)
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
    , ToChimericAccount (..)
    , WalletKey (..)
    , deriveRewardAccount
    , fromHex
    , hex
    , mutableAccount
    , networkDiscriminantVal
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( DerivationPrefix (..)
    , SeqState (..)
    , coinTypeAda
    , purposeCIP1852
    , rewardAccountKey
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..), invariant )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( guard, when, (<=<) )
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
    ( Word8 )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE

{-------------------------------------------------------------------------------
                            Sequential Derivation
-------------------------------------------------------------------------------}

-- | A cryptographic key for Jormungandr address derivation, with phantom-types to
-- disambiguate derivation paths
--
-- @
-- let rootPrivateKey = JormungandrKey 'RootK XPrv
-- let accountPubKey = JormungandrKey 'AccountK XPub
-- let addressPubKey = JormungandrKey 'AddressK XPub
-- @
newtype JormungandrKey (depth :: Depth) key =
    JormungandrKey { getKey :: key }
    deriving stock (Generic, Show, Eq)

instance (NFData key) => NFData (JormungandrKey depth key)

-- | Size, in bytes, of a public key (without chain code)
publicKeySize :: Int
publicKeySize = 32

-- Serialized length in bytes of a Single Address
addrSingleSize :: Int
addrSingleSize = 1 + publicKeySize

-- Serialized length in bytes of a Grouped Address
addrGroupedSize :: Int
addrGroupedSize = addrSingleSize + publicKeySize

-- | The minimum seed length for 'generateKeyFromSeed' and
-- 'unsafeGenerateKeyFromSeed'.
minSeedLengthBytes :: Int
minSeedLengthBytes = 16

-- | Generate a root key from a corresponding seed.
-- The seed should be at least 16 bytes.
generateKeyFromSeed
    :: (SomeMnemonic, Maybe SomeMnemonic)
       -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> JormungandrKey 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed

-- | Generate a new key from seed. Note that the @depth@ is left open so that
-- the caller gets to decide what type of key this is. This is mostly for
-- testing, in practice, seeds are used to represent root keys, and one should
-- use 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: (SomeMnemonic, Maybe SomeMnemonic)
        -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> JormungandrKey depth XPrv
unsafeGenerateKeyFromSeed (root, m2nd) (Passphrase pwd) =
    JormungandrKey $ generateNew seed' (maybe mempty mnemonicToBytes m2nd) pwd
  where
    mnemonicToBytes (SomeMnemonic mw) = entropyToBytes $ mnemonicToEntropy mw
    seed  = mnemonicToBytes root
    seed' = invariant
        ("seed length : " <> show (BA.length seed) <> " in (Passphrase \"seed\") is not valid")
        seed
        (\s -> BA.length s >= minSeedLengthBytes && BA.length s <= 255)


instance HardDerivation JormungandrKey where
    type AddressIndexDerivationType JormungandrKey = 'Soft

    deriveAccountPrivateKey
            (Passphrase pwd) (JormungandrKey rootXPrv) (Index accIx) =
        let
            purposeXPrv = -- lvl1 derivation; hardened derivation of purpose'
                deriveXPrv DerivationScheme2 pwd rootXPrv (getIndex purposeCIP1852)
            coinTypeXPrv = -- lvl2 derivation; hardened derivation of coin_type'
                deriveXPrv DerivationScheme2 pwd purposeXPrv (getIndex coinTypeAda)
            acctXPrv = -- lvl3 derivation; hardened derivation of account' index
                deriveXPrv DerivationScheme2 pwd coinTypeXPrv accIx
        in
            JormungandrKey acctXPrv

    deriveAddressPrivateKey
            (Passphrase pwd) (JormungandrKey accXPrv) accountingStyle (Index addrIx) =
        let
            changeCode =
                fromIntegral $ fromEnum accountingStyle
            changeXPrv = -- lvl4 derivation; soft derivation of change chain
                deriveXPrv DerivationScheme2 pwd accXPrv changeCode
            addrXPrv = -- lvl5 derivation; soft derivation of address index
                deriveXPrv DerivationScheme2 pwd changeXPrv addrIx
        in
            JormungandrKey addrXPrv

instance SoftDerivation JormungandrKey where
    deriveAddressPublicKey (JormungandrKey accXPub) accountingStyle (Index addrIx) =
        fromMaybe errWrongIndex $ do
            let changeCode = fromIntegral $ fromEnum accountingStyle
            changeXPub <- -- lvl4 derivation in bip44 is derivation of change chain
                deriveXPub DerivationScheme2 accXPub changeCode
            addrXPub <- -- lvl5 derivation in bip44 is derivation of address chain
                deriveXPub DerivationScheme2 changeXPub addrIx
            return $ JormungandrKey addrXPub
      where
        errWrongIndex = error $
            "deriveAddressPublicKey failed: was given an hardened (or too big) \
            \index for soft path derivation ( " ++ show addrIx ++ "). This is \
            \either a programmer error, or, we may have reached the maximum \
            \number of addresses for a given wallet."

{-------------------------------------------------------------------------------
                            WalletKey implementation
-------------------------------------------------------------------------------}

instance WalletKey JormungandrKey where
    changePassphrase = changePassphraseSeq
    publicKey = publicKeySeq
    digest = digestSeq
    getRawKey = getKey
    keyTypeDescriptor _ = "seq"

-- | Extract the public key part of a private key.
publicKeySeq
    :: JormungandrKey depth XPrv
    -> JormungandrKey depth XPub
publicKeySeq (JormungandrKey k) =
    JormungandrKey (toXPub k)

-- | Hash a public key to some other representation.
digestSeq
    :: HashAlgorithm a
    => JormungandrKey depth XPub
    -> Digest a
digestSeq (JormungandrKey k) =
    hash (unXPub k)

-- | Re-encrypt a private key using a different passphrase.
--
-- **Important**:
-- This function doesn't check that the old passphrase is correct! Caller is
-- expected to have already checked that. Using an incorrect passphrase here
-- will lead to very bad thing.
changePassphraseSeq
    :: Passphrase "encryption"
    -> Passphrase "encryption"
    -> JormungandrKey depth XPrv
    -> JormungandrKey depth XPrv
changePassphraseSeq (Passphrase oldPwd) (Passphrase newPwd) (JormungandrKey prv) =
    JormungandrKey $ xPrvChangePass oldPwd newPwd prv

{-------------------------------------------------------------------------------
                         Relationship Key / Address
-------------------------------------------------------------------------------}

instance PaymentAddress 'Mainnet JormungandrKey where
    paymentAddress (JormungandrKey k0) =
        encodeJormungandrAddress (addrSingle @'Mainnet) [xpubPublicKey k0]
    liftPaymentAddress (KeyFingerprint k0) =
        encodeJormungandrAddress (addrSingle @'Mainnet) [k0]

instance PaymentAddress ('Testnet pm) JormungandrKey where
    paymentAddress (JormungandrKey k0) =
        encodeJormungandrAddress (addrSingle @('Testnet _)) [xpubPublicKey k0]
    liftPaymentAddress (KeyFingerprint k0) =
        encodeJormungandrAddress (addrSingle @('Testnet _)) [k0]

instance DelegationAddress 'Mainnet JormungandrKey where
    delegationAddress (JormungandrKey k0) (JormungandrKey k1) =
        encodeJormungandrAddress (addrGrouped @'Mainnet) (xpubPublicKey <$> [k0, k1])
    liftDelegationAddress (KeyFingerprint k0) (JormungandrKey k1) =
        encodeJormungandrAddress (addrGrouped @'Mainnet) ([k0, xpubPublicKey k1])

instance DelegationAddress ('Testnet pm) JormungandrKey where
    delegationAddress (JormungandrKey k0) (JormungandrKey k1) =
        encodeJormungandrAddress (addrGrouped @('Testnet _)) (xpubPublicKey <$> [k0, k1])
    liftDelegationAddress (KeyFingerprint k0) (JormungandrKey k1) =
        encodeJormungandrAddress (addrGrouped @('Testnet _)) ([k0, xpubPublicKey k1])

-- | Embed some constants into a network type.
class KnownNetwork (n :: NetworkDiscriminant) where
    addrSingle :: Word8
        -- ^ Address discriminant byte for single addresses, this is the first
        -- byte of every addresses using the Jormungandr format carrying only a
        -- spending key.

    addrGrouped :: Word8
        -- ^ Address discriminant byte for grouped addresses, this is the first
        -- byte of every addresses using the Jormungandr format carrying both a
        -- spending and an account key.

    addrAccount :: Word8
        -- ^ Address discriminant byte for account addresses, this is the first
        -- byte of every addresses using the Jormungandr format carrying only an
        -- account key.

    knownDiscriminants :: [Word8]
    knownDiscriminants =
        [ addrSingle @n
        , addrGrouped @n
        , addrAccount @n
        ]

instance KnownNetwork 'Mainnet where
    addrSingle = 0x03
    addrGrouped = 0x04
    addrAccount = 0x05

instance KnownNetwork ('Testnet pm) where
    addrSingle = 0x83
    addrGrouped = 0x84
    addrAccount = 0x85

isAddrSingle :: Address -> Bool
isAddrSingle (Address bytes) =
    firstByte `elem` [[addrSingle @('Testnet _)], [addrSingle @'Mainnet]]
  where
    firstByte = BS.unpack (BS.take 1 bytes)

isAddrGrouped :: Address -> Bool
isAddrGrouped (Address bytes) =
    firstByte `elem` [[addrGrouped @('Testnet _)], [addrGrouped @'Mainnet]]
  where
    firstByte = BS.unpack (BS.take 1 bytes)

-- | Internal function to encode shelley addresses from key fingerprints.
--
-- We use this to define both instance separately to avoid having to carry a
-- 'KnownNetwork' constraint around.
encodeJormungandrAddress :: Word8 -> [ByteString] -> Address
encodeJormungandrAddress discriminant keys =
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
decodeJormungandrAddress
    :: forall n. (KnownNetwork n, NetworkDiscriminantVal n)
    => ByteString
    -> Either TextDecodingError Address
decodeJormungandrAddress bytes = do
    let firstByte = BS.unpack $ BS.take 1 bytes

    let knownBytes = pure <$>
            knownDiscriminants @('Testnet _) ++ knownDiscriminants @'Mainnet
    when (firstByte `notElem` knownBytes) $ Left (invalidAddressType firstByte)

    let allowedBytes = pure <$> knownDiscriminants @n
    when (firstByte `notElem` allowedBytes) $ Left invalidNetwork

    case BS.length bytes of
        n | n == addrSingleSize  && firstByte /= [addrGrouped @n] -> pure ()
        n | n == addrGroupedSize && firstByte == [addrGrouped @n] -> pure ()
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
    invalidAddressType byte = TextDecodingError $
        "This type of address is not supported: "
        <> show byte
        <> "."
    invalidNetwork = TextDecodingError $
        "This address belongs to another network. Network is: "
        <> show (networkDiscriminantVal @n) <> "."

instance MkKeyFingerprint JormungandrKey Address where
    paymentKeyFingerprint addr@(Address bytes)
        | isAddrSingle addr || isAddrGrouped addr =
            Right $ KeyFingerprint $ BS.take publicKeySize $ BS.drop 1 bytes
        | otherwise =
            Left $ ErrInvalidAddress addr (Proxy @JormungandrKey)

instance MkKeyFingerprint JormungandrKey (Proxy (n :: NetworkDiscriminant), JormungandrKey 'AddressK XPub) where
    paymentKeyFingerprint =
        Right . KeyFingerprint . xpubPublicKey . getRawKey . snd

{-------------------------------------------------------------------------------
                          Dealing with Rewards
-------------------------------------------------------------------------------}

instance IsOurs (SeqState n JormungandrKey) ChimericAccount
  where
    isOurs account state@SeqState{derivationPrefix} =
        let
            DerivationPrefix (purpose, coinType, accountIx) = derivationPrefix
            path = NE.fromList
                [ DerivationIndex $ getIndex purpose
                , DerivationIndex $ getIndex coinType
                , DerivationIndex $ getIndex accountIx
                , DerivationIndex $ getIndex mutableAccount
                , DerivationIndex $ getIndex @'Soft minBound
                ]
        in
            (guard (account == ourAccount) *> Just path, state)
      where
        ourAccount = toChimericAccount $ rewardAccountKey state

instance ToChimericAccount JormungandrKey where
    toChimericAccount = ChimericAccount . xpubPublicKey . getKey
    someChimericAccount mw =
        (getRawKey acctK, toChimericAccount (publicKey acctK))
      where
        rootK = generateKeyFromSeed (mw, Nothing) mempty
        acctK = deriveRewardAccount mempty rootK

{-------------------------------------------------------------------------------
                          Storing and retrieving keys
-------------------------------------------------------------------------------}

instance PersistPrivateKey (JormungandrKey 'RootK) where
    serializeXPrv (k, h) =
        ( hex . unXPrv . getKey $ k
        , hex . getHash $ h
        )

    unsafeDeserializeXPrv (k, h) = either err id $ (,)
        <$> fmap JormungandrKey (xprvFromText k)
        <*> fmap Hash (fromHex h)
      where
        xprvFromText = xprv <=< fromHex @ByteString
        err _ = error "unsafeDeserializeXPrv: unable to deserialize JormungandrKey"

instance PersistPublicKey (JormungandrKey depth) where
    serializeXPub =
        hex . unXPub . getKey

    unsafeDeserializeXPub =
        either err JormungandrKey . xpubFromText
      where
        err _ = error "unsafeDeserializeXPub: unable to deserialize JormungandrKey"

xpubFromText :: ByteString -> Either String XPub
xpubFromText = xpub <=< fromHex @ByteString

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
-- - 'publicKey' --> For any @JormungandrKey _ XPrv@ to @JormungandrKey _ XPub@
-- - 'deriveAccountPrivateKey' -->
--      From @JormungandrKey RootK XPrv@ to @JormungandrKey AccountK XPrv@
-- - 'deriveAddressPrivateKey' -->
--      From @JormungandrKey AccountK XPrv@ to @JormungandrKey AddressK XPrv@
-- - 'deriveAddressPublicKey' -->
--      From @JormungandrKey AccountK XPub@ to @JormungandrKey AddressK XPub@
--
-- For example:
--
-- @
-- -- Access public key for: m|coinType'|purpose'|0'
-- let seed = "My Secret Seed"
--
-- let rootXPrv :: JormungandrKey 'RootK XPrv
--     rootXPrv = generateKeyFromSeed (seed, mempty) mempty
--
-- let accIx :: Index 'Hardened 'AccountK
--     accIx = toEnum 0x80000000
--
-- let accXPub :: JormungandrKey 'AccountL XPub
--     accXPub = publicKey $ deriveAccountPrivateKey mempty rootXPrv accIx
-- @
