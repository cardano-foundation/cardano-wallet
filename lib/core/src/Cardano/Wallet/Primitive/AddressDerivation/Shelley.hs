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
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Implementation of address derivation for 'Shelley' Keys.

module Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( -- * Types
      ShelleyKey(..)

    -- * Constants
    , minSeedLengthBytes


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
import Cardano.Mnemonic
    ( SomeMnemonic (..), entropyToBytes, mnemonicToEntropy )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PaymentAddress (..)
    , PersistPrivateKey (..)
    , PersistPublicKey (..)
    , SoftDerivation (..)
    , WalletKey (..)
    , fromHex
    , hex
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Hash (..), invariant )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( (<=<) )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256 (..) )
import Crypto.Hash.IO
    ( HashAlgorithm (hashDigestSize) )
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
    ( Word32 )
import GHC.Generics
    ( Generic )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

{-------------------------------------------------------------------------------
                            Sequential Derivation
-------------------------------------------------------------------------------}

-- | A cryptographic key for Shelley address derivation, with phantom-types to
-- disambiguate derivation paths
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

-- | Purpose is a constant set to 1852' (or 0x8000073c) following the BIP-44
-- extension for Cardano:
--
-- https://github.com/input-output-hk/implementation-decisions/blob/e2d1bed5e617f0907bc5e12cf1c3f3302a4a7c42/text/1852-hd-chimeric.md
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeIndex :: Word32
purposeIndex = 0x8000073c

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
    :: (SomeMnemonic, Maybe SomeMnemonic)
       -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> ShelleyKey 'RootK XPrv
generateKeyFromSeed = unsafeGenerateKeyFromSeed

-- | Generate a new key from seed. Note that the @depth@ is left open so that
-- the caller gets to decide what type of key this is. This is mostly for
-- testing, in practice, seeds are used to represent root keys, and one should
-- use 'generateKeyFromSeed'.
unsafeGenerateKeyFromSeed
    :: (SomeMnemonic, Maybe SomeMnemonic)
        -- ^ The actual seed and its recovery / generation passphrase
    -> Passphrase "encryption"
    -> ShelleyKey depth XPrv
unsafeGenerateKeyFromSeed (root, m2nd) (Passphrase pwd) =
    ShelleyKey $ generateNew seed' (maybe mempty mnemonicToBytes m2nd) pwd
  where
    mnemonicToBytes (SomeMnemonic mw) = entropyToBytes $ mnemonicToEntropy mw
    seed  = mnemonicToBytes root
    seed' = invariant
        ("seed length : " <> show (BA.length seed) <> " in (Passphrase \"seed\") is not valid")
        seed
        (\s -> BA.length s >= minSeedLengthBytes && BA.length s <= 255)


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
    changePassphrase (Passphrase oldPwd) (Passphrase newPwd) (ShelleyKey prv) =
        ShelleyKey $ xPrvChangePass oldPwd newPwd prv

    publicKey (ShelleyKey prv) =
        ShelleyKey (toXPub prv)

    digest (ShelleyKey pub) =
        hash (unXPub pub)

    getRawKey =
        getKey

    keyTypeDescriptor _ =
        "she"

{-------------------------------------------------------------------------------
                         Relationship Key / Address
-------------------------------------------------------------------------------}

instance PaymentAddress 'Mainnet ShelleyKey where
    paymentAddress paymentK = do
        Address $ BL.toStrict $ runPut $ do
            putWord8 (enterprise + networkId)
            putByteString . blake2b256 $ paymentK
      where
        enterprise = 96
        networkId = 1

    liftPaymentAddress (KeyFingerprint fingerprint) =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (enterprise + networkId)
            putByteString fingerprint
      where
        enterprise = 96
        networkId = 1

instance PaymentAddress ('Testnet pm) ShelleyKey where
    paymentAddress paymentK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (enterprise + networkId)
            putByteString . blake2b256 $ paymentK
      where
        enterprise = 96
        networkId = 0

    liftPaymentAddress (KeyFingerprint fingerprint) =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (enterprise + networkId)
            putByteString fingerprint
      where
        enterprise = 96
        networkId = 0

instance DelegationAddress 'Mainnet ShelleyKey where
    delegationAddress paymentK stakingK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (base + networkId)
            putByteString . blake2b256 $ paymentK
            putByteString . blake2b256 $ stakingK
      where
        base = 0
        networkId = 1

    liftDelegationAddress (KeyFingerprint fingerprint) stakingK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (base + networkId)
            putByteString fingerprint
            putByteString . blake2b256 $ stakingK
      where
        base = 0
        networkId = 1

instance DelegationAddress ('Testnet pm) ShelleyKey where
    delegationAddress paymentK stakingK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (base + networkId)
            putByteString . blake2b256 $ paymentK
            putByteString . blake2b256 $ stakingK
      where
        base = 0
        networkId = 0

    liftDelegationAddress (KeyFingerprint fingerprint) stakingK =
        Address $ BL.toStrict $ runPut $ do
            putWord8 (base + networkId)
            putByteString fingerprint
            putByteString . blake2b256 $ stakingK
      where
        base = 0
        networkId = 1

-- | Verify the structure of a payload decoded from a Bech32 text string
decodeShelleyAddress
    :: ByteString
    -> Either TextDecodingError Address
decodeShelleyAddress _bytes = do
    error "TODO: decodeShelleyAddress"

instance MkKeyFingerprint ShelleyKey Address where
    paymentKeyFingerprint (Address bytes) =
        Right $ KeyFingerprint $ BS.take hashSize $ BS.drop 1 bytes

instance MkKeyFingerprint ShelleyKey (Proxy (n :: NetworkDiscriminant), ShelleyKey 'AddressK XPub) where
    paymentKeyFingerprint (_, paymentK) =
        Right $ KeyFingerprint $ blake2b256 paymentK

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

instance PersistPublicKey (ShelleyKey depth) where
    serializeXPub =
        hex . unXPub . getKey

    unsafeDeserializeXPub =
        either err ShelleyKey . (xpub <=< fromHex @ByteString)
      where
        err _ = error "unsafeDeserializeXPub: unable to deserialize ShelleyKey"

{-------------------------------------------------------------------------------
                                 Internals
-------------------------------------------------------------------------------}

-- FIXME: The final implementation should use Blake2b_224 hashes, but so far,
-- cardano-node still uses blake2b_256 hashes... so we do.
--
-- -- Size of a public key hash (length in bytes)
-- hashSize :: Int
-- hashSize =
--     hashDigestSize Blake2b_224
--
-- -- Hash a public key
-- blake2b224 :: ShelleyKey depth XPub -> ByteString
-- blake2b224 =
--     BA.convert . hash @_ @Blake2b_224 . xpubPublicKey . getKey

-- Size of a public key hash (length in bytes)
hashSize :: Int
hashSize =
    hashDigestSize Blake2b_256

-- Hash a public key
blake2b256 :: ShelleyKey depth XPub -> ByteString
blake2b256 =
    BA.convert . hash @_ @Blake2b_256 . xpubPublicKey . getKey
