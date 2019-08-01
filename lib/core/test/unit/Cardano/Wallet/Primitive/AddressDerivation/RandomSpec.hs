{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivation.RandomSpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub, toXPub, xprv )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index
    , Index (..)
    , Passphrase (..)
    , XPrv
    , fromMnemonic
    )
import Cardano.Wallet.Primitive.AddressDerivation.Common
    ( Key (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( addrToPayload
    , decodeAddressDerivationPath
    , decodeDerivationPath
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deserialise
    , encodeDerivationPath
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
import Cardano.Wallet.Primitive.Types
    ( Address (..) )
import Cardano.Wallet.Unsafe
    ( unsafeDeserialiseFromBytes )
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
    ( Arbitrary (..)
    , Property
    , choose
    , property
    , vectorOf
    , (.&&.)
    , (===)
    , (==>)
    )

import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Random Address Derivation Properties" $ do
        it "Key derivation works for various indexes" $
            property prop_keyDerivation

        it "Address Derivation Path roundtrip" $ do
            property prop_derivationPathRoundTrip

    goldenSpec

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_keyDerivation
    :: Passphrase "seed"
    -> Passphrase "encryption"
    -> Index 'Hardened 'AccountK
    -> Index 'Soft 'AddressK
    -> Property
prop_keyDerivation seed encPwd ix1 ix2 =
    addrXPrv `seq` property () -- NOTE Making sure this doesn't throw
  where
    rootXPrv = generateKeyFromSeed seed encPwd
    accXPrv = deriveAccountPrivateKey encPwd rootXPrv ix1
    addrXPrv = deriveAddressPrivateKey encPwd accXPrv ix2

prop_derivationPathRoundTrip
    :: Key 'RootK XPub
    -> Key 'RootK XPub
    -> Index 'Hardened 'AccountK
    -> Index 'Soft 'AddressK
    -> Property
prop_derivationPathRoundTrip rk rk' accIx addrIx =
    let
        encoded = CBOR.toLazyByteString $ encodeDerivationPath rk accIx addrIx
        decoded = unsafeDeserialiseFromBytes (decodeDerivationPath rk) encoded
        decoded' = unsafeDeserialiseFromBytes (decodeDerivationPath rk') encoded
    in
        decoded === Just (accIx, addrIx) .&&.
        (rk /= rk' ==> decoded' === Nothing)

{-------------------------------------------------------------------------------
                                  Golden tests
-------------------------------------------------------------------------------}

goldenSpec :: Spec
goldenSpec = describe "Golden tests" $ do
    it "generateKeyFromSeed - no passphrase" $
        generateTest generateTest1

    it "generateKeyFromSeed - with passphrase" $
        generateTest generateTest2

    it "decodeDerivationPath - mainnet - initial account" $
        decodeTest decodeTest1

    it "decodeDerivationPath - mainnet - another account" $
        decodeTest decodeTest2

    it "decodeDerivationPath - testnet - initial account" $
        decodeTest decodeTest3

    it "decodeDerivationPath - testnet - another account" $
        decodeTest decodeTest4

{-------------------------------------------------------------------------------
                      Golden tests for generateKeyFromSeed
-------------------------------------------------------------------------------}

data GenerateKeyFromSeed = GenerateKeyFromSeed
    { mnem :: [Text]
    , pwd :: Passphrase "encryption"
    , rootKey :: Key 'RootK XPrv
    } deriving (Show, Eq)

generateTest :: GenerateKeyFromSeed -> Expectation
generateTest GenerateKeyFromSeed{..} =
    generateKeyFromSeed (Passphrase seed) pwd `shouldBe` rootKey
  where
    Right (Passphrase seed) = fromMnemonic @'[12] mnem

generateTest1 :: GenerateKeyFromSeed
generateTest1 = GenerateKeyFromSeed
    { mnem = defMnemonic
    , pwd = pp ""
    , rootKey = xprv16 "b84d0b6db447911a98a3ade98145c0e8323e106f07bc17a99c2104c2688bb7528310902a3cec7e262ded6a4369ec1f48966a6b48b1ee90aa00e61b95417949f81258854ab44b0cfda59bd68fbd87f280841a390068049df0f8a903c94ba65b7aa4762129a6c83acfda5b257eaeb73ec5fee1518b6674fdc7891fe23f06174421"
    }

generateTest2 :: GenerateKeyFromSeed
generateTest2 = GenerateKeyFromSeed
    { mnem = defMnemonic
    , pwd = pp "4a87f05fe25a57c96ff5221863e61b91bcca566b853b616f55e5d2c18caa1a4c"
    , rootKey = xprv16 "b842ae13cbb31b7d96910472bbed5c8729c764d66af81b48120a6a583eae55faf78c24765e0c9826f4d095f3e6addb4bda68df322b220d3c08b8a5b414232d101258854ab44b0cfda59bd68fbd87f280841a390068049df0f8a903c94ba65b7aa4762129a6c83acfda5b257eaeb73ec5fee1518b6674fdc7891fe23f06174421"
    }

{- test data was generated with the following:
genKey :: ByteString -> ByteString
genKey spendingPassword = convertToBase Base16 (unXPrv prv)
  where
    spendingPassword' = PassPhrase pp
    Right pp = convertFromBase Base16 spendingPassword
    mnemonic = def :: Mnemonic 12
    (_, EncryptedSecretKey prv _hash) =
        safeDeterministicKeyGen seed spendingPassword'
    seed = Mnemonic.mnemonicToSeed mnemonic
-}

-- | This is the mnemonic that provides the 'Default' instance in cardano-sl
defMnemonic :: [Text]
defMnemonic =
    [ "squirrel", "material", "silly", "twice", "direct", "slush"
    , "pistol", "razor", "become", "junk", "kingdom", "flee" ]

{-------------------------------------------------------------------------------
                    Golden tests for Address derivation path
-------------------------------------------------------------------------------}

decodeTest :: DecodeDerivationPath -> Expectation
decodeTest DecodeDerivationPath{..} =
    decoded `shouldBe` Right (Just (Index accIndex, Index addrIndex))
  where
    decoded = deserialise (decodeAddressDerivationPath rootXPub) (addrToPayload $ Address addr)
    Key rootXPrv = generateKeyFromSeed (Passphrase seed) (Passphrase "")
    rootXPub = Key (toXPub rootXPrv)
    Right (Passphrase seed) = fromMnemonic @'[12] mnem

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

-- | Get a private key from a hex string, without error checking.
xprv16 :: ByteString -> Key purpose XPrv
xprv16 hex = Key k
  where
    Right k = xprvFromText hex
    xprvFromText = xprv <=< fromHexText
    fromHexText :: ByteString -> Either String ByteString
    fromHexText = convertFromBase Base16

-- | Get a passphrase from a hex string, without error checking
pp :: ByteString -> Passphrase purpose
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
