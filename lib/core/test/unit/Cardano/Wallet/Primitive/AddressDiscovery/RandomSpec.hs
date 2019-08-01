{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDiscovery.RandomSpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( toXPub, unXPrv )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), DerivationType (..), Index (..), Passphrase (..), XPrv )
import Cardano.Wallet.Primitive.AddressDerivation.Common
    ( Key (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , generateKeyFromSeed
    , minSeedLengthBytes
    , toAddress
    )
import Cardano.Wallet.Primitive.AddressDerivationSpec
    ()
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Property, choose, property, vectorOf, (.&&.), (===) )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Random Address Discovery Properties" $ do

        it "isOurs works as expected during key derivation" $ do
            property prop_keyDerivationObeysIsOurs


{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_keyDerivationObeysIsOurs
    :: Passphrase "seed"
    -> Passphrase "encryption"
    -> Index 'Hardened 'AccountK
    -> Index 'Soft 'AddressK
    -> Key 'RootK XPrv
    -> Property
prop_keyDerivationObeysIsOurs seed encPwd accIx addrIx rk' =
    isOurs address (RndState rootXPrv) === (True, RndState rootXPrv) .&&.
    isOurs address (RndState rk') === (False, RndState rk')
  where
    rootXPrv@(Key rootXPrv') = generateKeyFromSeed seed encPwd
    rootXPub = Key $ toXPub rootXPrv'
    accXPrv = deriveAccountPrivateKey encPwd rootXPrv accIx
    (Key addrXPrv) = deriveAddressPrivateKey encPwd accXPrv addrIx
    addrXPub = Key $ toXPub addrXPrv
    address = toAddress rootXPub addrXPub accIx addrIx

instance Eq RndState where
    (RndState (Key a)) == (RndState (Key b)) = unXPrv a == unXPrv b

instance Show RndState where
    show (RndState (Key a)) = show (unXPrv a)

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
