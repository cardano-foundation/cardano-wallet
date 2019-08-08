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
    ( Passphrase (..), fromMnemonic )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey (..), generateKeyFromSeed, minSeedLengthBytes )
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
    ( Expectation, Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), choose, vectorOf )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

spec :: Spec
spec = goldenSpec

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
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Eq RndState where
    (RndState a) == (RndState b) = getKey a == getKey b

instance Show RndState where
    show (RndState a) = show (getKey a)


-- This generator will only produce valid (@>= minSeedLengthBytes@) passphrases
-- because 'generateKeyFromSeed' is a partial function.
instance {-# OVERLAPS #-} Arbitrary (Passphrase "seed") where
    arbitrary = do
        n <- choose (minSeedLengthBytes, 64)
        bytes <- BS.pack <$> vectorOf n arbitrary
        return $ Passphrase $ BA.convert bytes
