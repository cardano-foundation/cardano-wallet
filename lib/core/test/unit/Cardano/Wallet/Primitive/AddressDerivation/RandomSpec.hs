{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDerivation.RandomSpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( xprv )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index
    , Passphrase (..)
    , XPrv
    , fromMnemonic
    )
import Cardano.Wallet.Primitive.AddressDerivation.Common
    ( Key (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( deriveAccountPrivateKey, deriveAddressPrivateKey, generateKeyFromSeed )
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
    describe "Random Address Derivation Properties" $ do
        it "Key derivation works for various indexes" $
            property prop_keyDerivation

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

{-------------------------------------------------------------------------------
                                  Golden tests
-------------------------------------------------------------------------------}

goldenSpec :: Spec
goldenSpec = describe "Golden tests" $ do
    it "generateKeyFromSeed - no passphrase" $
        generateTest test1

    it "generateKeyFromSeed - with passphrase" $
        generateTest test2

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

test1 :: GenerateKeyFromSeed
test1 = GenerateKeyFromSeed
    { mnem = defMnem
    , pwd = pp ""
    , rootKey = xprv16 "b84d0b6db447911a98a3ade98145c0e8323e106f07bc17a99c2104c2688bb7528310902a3cec7e262ded6a4369ec1f48966a6b48b1ee90aa00e61b95417949f81258854ab44b0cfda59bd68fbd87f280841a390068049df0f8a903c94ba65b7aa4762129a6c83acfda5b257eaeb73ec5fee1518b6674fdc7891fe23f06174421"
    }

test2 :: GenerateKeyFromSeed
test2 = GenerateKeyFromSeed
    { mnem = defMnem
    , pwd = pp "4a87f05fe25a57c96ff5221863e61b91bcca566b853b616f55e5d2c18caa1a4c"
    , rootKey = xprv16 "c7afbf0c46212c44c5b6a10cf2e7e0a94140f792b8f58964b17a7a67b761619aeed21598a7dde167e0e34f05b5b72c9b61248f4374d858751482f154eea106511258854ab44b0cfda59bd68fbd87f280841a390068049df0f8a903c94ba65b7aa4762129a6c83acfda5b257eaeb73ec5fee1518b6674fdc7891fe23f06174421"
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
defMnem :: [Text]
defMnem = [ "squirrel", "material", "silly", "twice", "direct", "slush"
          , "pistol", "razor", "become", "junk", "kingdom", "flee" ]

-- Get a private key from a hex string, without error checking.
xprv16 :: ByteString -> Key purpose XPrv
xprv16 hex = Key k
  where
    Right k = xprvFromText hex
    xprvFromText = xprv <=< fromHexText
    fromHexText :: ByteString -> Either String ByteString
    fromHexText = convertFromBase Base16

-- Get a passphrase from a hex string, without error checking
pp :: ByteString -> Passphrase purpose
pp hex = Passphrase b
    where Right b = convertFromBase Base16 hex


{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance {-# OVERLAPS #-} Arbitrary (Passphrase "seed") where
    arbitrary = do
        n <- choose (32, 64)
        bytes <- BS.pack <$> vectorOf n arbitrary
        return $ Passphrase $ BA.convert bytes
