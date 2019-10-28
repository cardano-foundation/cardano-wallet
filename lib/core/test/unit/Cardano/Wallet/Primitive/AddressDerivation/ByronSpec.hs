{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivation.ByronSpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( xprv )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index
    , Index (..)
    , Passphrase (..)
    , XPrv
    , fromMnemonic
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..)
    , generateKeyFromSeed
    , minSeedLengthBytes
    , unsafeGenerateKeyFromSeed
    )

import Cardano.Wallet.Primitive.AddressDerivationSpec
    ()
import Control.Monad
    ( (<=<) )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), Property, choose, property, vectorOf )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    goldenSpec
    describe "Random Address Derivation Properties" $ do
        it "Key derivation works for various indexes" $
            property prop_keyDerivation

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_keyDerivation
    :: Passphrase "seed"
    -> Passphrase "encryption"
    -> Index 'Hardened 'AccountK
    -> Index 'Hardened 'AddressK
    -> Property
prop_keyDerivation seed encPwd accIx addrIx =
    rndKey `seq` property () -- NOTE Making sure this doesn't throw
  where
    rndKey :: ByronKey 'AddressK XPrv
    rndKey = unsafeGenerateKeyFromSeed (accIx, addrIx) seed encPwd

{-------------------------------------------------------------------------------
                                  Golden tests
-------------------------------------------------------------------------------}

goldenSpec :: Spec
goldenSpec = describe "Golden tests" $ do
    it "generateKeyFromSeed - no passphrase" $
        generateTest generateTest1

    it "generateKeyFromSeed - with passphrase" $
        generateTest generateTest2

{-------------------------------------------------------------------------------
                      Golden tests for generateKeyFromSeed
-------------------------------------------------------------------------------}

data GenerateKeyFromSeed = GenerateKeyFromSeed
    { mnem :: [Text]
    , pwd :: Passphrase "encryption"
    , rootKey :: ByronKey 'RootK XPrv
    }

generateTest :: GenerateKeyFromSeed -> Expectation
generateTest GenerateKeyFromSeed{..} =
    getKey (generateKeyFromSeed (Passphrase seed) pwd)
    `shouldBe`
    getKey rootKey
  where
    Right (Passphrase seed) = fromMnemonic @'[12] mnem

generateTest1 :: GenerateKeyFromSeed
generateTest1 = GenerateKeyFromSeed
    { mnem = defMnemonic
    , pwd = pp ""
    , rootKey = xprv16
        "b84d0b6db447911a98a3ade98145c0e8323e106f07bc17a99c2104c2688bb752831090\
        \2a3cec7e262ded6a4369ec1f48966a6b48b1ee90aa00e61b95417949f81258854ab44b\
        \0cfda59bd68fbd87f280841a390068049df0f8a903c94ba65b7aa4762129a6c83acfda\
        \5b257eaeb73ec5fee1518b6674fdc7891fe23f06174421"
    }

generateTest2 :: GenerateKeyFromSeed
generateTest2 = GenerateKeyFromSeed
    { mnem = defMnemonic
    , pwd = pp "4a87f05fe25a57c96ff5221863e61b91bcca566b853b616f55e5d2c18caa1a4c"
    , rootKey = xprv16
        "b842ae13cbb31b7d96910472bbed5c8729c764d66af81b48120a6a583eae55faf78c247\
        \65e0c9826f4d095f3e6addb4bda68df322b220d3c08b8a5b414232d101258854ab44b0c\
        \fda59bd68fbd87f280841a390068049df0f8a903c94ba65b7aa4762129a6c83acfda5b2\
        \57eaeb73ec5fee1518b6674fdc7891fe23f06174421"
    }

-- | This is the mnemonic that provides the 'Default' instance in cardano-sl
defMnemonic :: [Text]
defMnemonic =
    [ "squirrel", "material", "silly", "twice", "direct", "slush"
    , "pistol", "razor", "become", "junk", "kingdom", "flee" ]

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

-- | Get a private key from a hex string, without error checking.
xprv16 :: ByteString -> ByronKey 'RootK XPrv
xprv16 hex = ByronKey k () (error "passphrase not used for tests")
  where
    Right k = xprvFromText hex
    xprvFromText = xprv <=< fromHexText
    fromHexText :: ByteString -> Either String ByteString
    fromHexText = convertFromBase Base16

-- | Get a passphrase from a hex string, without error checking
pp :: ByteString -> Passphrase depth
pp hex = Passphrase b
    where Right b = convertFromBase Base16 hex

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
