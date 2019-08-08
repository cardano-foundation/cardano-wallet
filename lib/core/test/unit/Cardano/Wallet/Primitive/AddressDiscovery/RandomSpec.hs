{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDiscovery.RandomSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , Passphrase (..)
    , XPrv
    , fromMnemonic
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey (..)
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , generateKeyFromSeed
    , minSeedLengthBytes
    )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ( DecodeDerivationPath (..)
    , decodeTest1
    , decodeTest2
    , decodeTest3
    , decodeTest4
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..) )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe, xit )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , Property
    , choose
    , property
    , vectorOf
    , (.&&.)
    , (===)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Random Address Discovery Properties" $ do

        xit "isOurs works as expected during key derivation" $ do
            property prop_keyDerivationObeysIsOurs

    goldenSpec


{-------------------------------------------------------------------------------
                                  Golden tests
-------------------------------------------------------------------------------}

goldenSpec :: Spec
goldenSpec = describe "Golden tests" $ do

    it "check isOurs - mainnet - initial account" $
        checkIsOurs decodeTest1

    it "check isOurs - mainnet - another account" $
        checkIsOurs decodeTest2

    it "check isOurs - testnet - initial account" $
        checkIsOurs decodeTest3

    it "check isOurs - testnet - another account" $
        checkIsOurs decodeTest4

checkIsOurs :: DecodeDerivationPath -> Expectation
checkIsOurs DecodeDerivationPath{..} =
    isOurs (Address addr) (RndState rootXPrv) `shouldBe` (True, RndState rootXPrv)
  where
    rootXPrv = generateKeyFromSeed (Passphrase seed) (Passphrase "")
    Right (Passphrase seed) = fromMnemonic @'[12] mnem


{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_keyDerivationObeysIsOurs
    :: Passphrase "seed"
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
    _rootXPub = publicKey rootXPrv
    accXPrv = deriveAccountPrivateKey encPwd rootXPrv accIx
    addrXPrv = deriveAddressPrivateKey encPwd accXPrv addrIx
    _addrXPub = publicKey addrXPrv
    address = undefined -- here use -> keyToAddress rootXPub addrXPub accIx addIx @Network

instance Eq RndState where
    (RndState a) == (RndState b) = getKey a == getKey b

instance Show RndState where
    show (RndState a) = show (getKey a)

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

-- This generator will only produce valid (@>= minSeedLengthBytes@) passphrases
-- because 'generateKeyFromSeed' is a partial function.
instance {-# OVERLAPS #-} Arbitrary (Passphrase "seed") where
    arbitrary = do
        n <- choose (minSeedLengthBytes, 64)
        bytes <- BS.pack <$> vectorOf n arbitrary
        return $ Passphrase $ BA.convert bytes

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
