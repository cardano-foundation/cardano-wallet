{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.HttpBridge.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub, unXPrv )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, Network (..) )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , KeyToAddress (..)
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , XPrv
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , generateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), decodeAddress, encodeAddress )
import Control.Monad
    ( replicateM )
import Data.Digest.CRC32
    ( crc32 )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( TextDecodingError (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , Property
    , arbitraryBoundedEnum
    , arbitraryPrintableChar
    , choose
    , property
    , vectorOf
    , (.&&.)
    , (===)
    )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "Compatibility - HttpBridge (Mainnet)" $ do
        let proxy = Proxy @(HttpBridge 'Mainnet)
        it "decodeAddress . encodeAddress = pure" $ property $ \a ->
            decodeAddress proxy (encodeAddress proxy a) === Right a

        it "decodeAddress failure \"0000\"" $ do
            let err = "Unable to decode Address: expected Base58 encoding."
            decodeAddress proxy "0000" === Left (TextDecodingError err)

        it "decodeAddress failure \"EkxDbkPo\"" $ do
            let err = "Unable to decode Address: not a valid Byron address."
            decodeAddress proxy "EkxDbkPo" === Left (TextDecodingError err)

    describe "Compatibility - HttpBridge (Testnet)" $ do
        let proxy = Proxy @(HttpBridge 'Testnet)
        it "decodeAddress . encodeAddress = pure" $ property $ \a ->
            decodeAddress proxy (encodeAddress proxy a) === Right a

        it "decodeAddress failure \"0000\"" $ do
            let err = "Unable to decode Address: expected Base58 encoding."
            decodeAddress proxy "0000" === Left (TextDecodingError err)

        it "decodeAddress failure \"EkxDbkPo\"" $ do
            let err = "Unable to decode Address: not a valid Byron address."
            decodeAddress proxy "EkxDbkPo" === Left (TextDecodingError err)

    describe "Random Address Discovery Properties" $ do
        it "isOurs works as expected during key derivation in testnet" $ do
            property (prop_keyDerivationObeysIsOurs @'Testnet)
        it "isOurs works as expected during key derivation in mainnet" $ do
            property (prop_keyDerivationObeysIsOurs @'Mainnet)

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_keyDerivationObeysIsOurs
    :: forall (n :: Network). KnownNetwork n
    => Passphrase "seed"
    -> Passphrase "encryption"
    -> Index 'Hardened 'AccountK
    -> Index 'Hardened 'AddressK
    -> RndKey 'RootK XPrv
    -> Property
prop_keyDerivationObeysIsOurs seed encPwd accIx addrIx rk' =
    isOurs address (RndState rootXPrv) === (True, RndState rootXPrv) .&&.
    isOurs address (RndState rk') === (False, RndState rk')
  where
    rootXPrv = generateKeyFromSeed seed encPwd
    accXPrv = deriveAccountPrivateKey encPwd rootXPrv accIx
    addrXPrv = deriveAddressPrivateKey encPwd accXPrv addrIx
    addrXPub = publicKey addrXPrv
    address = (keyToAddress @(HttpBridge n)) addrXPub

instance Eq RndState where
    (RndState a) == (RndState b) = getKey a == getKey b

instance Show RndState where
    show (RndState a) = show (getKey a)

instance Show XPrv where
    show = show . unXPrv

instance Eq XPrv where
    a == b = unXPrv a == unXPrv b

instance Arbitrary Address where
    arbitrary = genAddress (30, 100)

instance Arbitrary (Index 'Hardened 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Passphrase goal) where
    shrink (Passphrase "") = []
    shrink (Passphrase _ ) = [Passphrase ""]
    arbitrary = do
        n <- choose (0, 32)
        InfiniteList bytes _ <- arbitrary
        return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

instance {-# OVERLAPS #-} Arbitrary (Passphrase "encryption") where
    arbitrary = do
        let p = Proxy :: Proxy "encryption"
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes


-- The address format on 'Staging' is the same as 'Mainnet'.
genAddress :: (Int, Int) -> Gen Address
genAddress range = do
    n <- choose range
    let prefix = BS.pack
            [ 130       -- Array(2)
            , 216, 24   -- Tag 24
            , 88, fromIntegral n -- Bytes(n), n > 23 && n < 256
            ]
    payload <- BS.pack <$> vectorOf n arbitrary
    let crc = CBOR.toStrictByteString (CBOR.encodeWord32 $ crc32 payload)
    return $ Address (prefix <> payload <> crc)

instance Arbitrary (RndKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = genRootKeys

genRootKeys :: Gen (RndKey 'RootK XPrv)
genRootKeys = do
    (s, e) <- (,)
        <$> genPassphrase @"seed" (16, 32)
        <*> genPassphrase @"encryption" (0, 16)
    return $ generateKeyFromSeed s e
  where
    genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
    genPassphrase range = do
        n <- choose range
        InfiniteList bytes _ <- arbitrary
        return $ Passphrase $ BA.convert $ BS.pack $ take n bytes
