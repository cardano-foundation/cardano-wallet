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
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey (..)
    ( addrToPayload
    , decodeAddressDerivationPath
    , decodeDerivationPath
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deserialise
    , encodeDerivationPath
    , generateKeyFromSeed
    , minSeedLengthBytes
    , unsafeDeserialiseFromBytes
    )

import Cardano.Wallet.Primitive.AddressDerivationSpec
    ()
import Cardano.Wallet.Primitive.Types
    ( Address (..) )
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
    rndKey :: RndKey 'AddressK XPrv
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
    , rootKey :: RndKey 'RootK XPrv
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
                    Golden tests for Address derivation path
-------------------------------------------------------------------------------}

data DecodeDerivationPath = DecodeDerivationPath
    { mnem :: [Text]
    , addr :: ByteString
    , accIndex :: Word32
    , addrIndex :: Word32
    } deriving (Generic, Show, Eq)

decodeTest :: DecodeDerivationPath -> Expectation
decodeTest DecodeDerivationPath{..} =
    decoded `shouldBe` Right (Just (Index accIndex, Index addrIndex))
  where
    decoded = deserialise (decodeAddressDerivationPath rootXPub) (addrToPayload $ Address addr)
    Key rootXPrv = generateKeyFromSeed (Passphrase seed) (Passphrase "")
    rootXPub = Key (toXPub rootXPrv)
    Right (Passphrase seed) = fromMnemonic @'[12] mnem

-- Generated on mainnet Daedalus -- first address of the initial account.
decodeTest1 :: DecodeDerivationPath
decodeTest1 = DecodeDerivationPath
    { mnem = addrMnemonic
    , addr = "DdzFFzCqrhsznTSvu5VS2Arte6DbsvfGL2mhezwj4T8fvJqQ4C53RYc8nrNukdpfUfxz3R5ryZTcMtFZfdq4hVkPFHD1XV2dxY7AJEon"
    , accIndex = 2147483648
    , addrIndex = 2147483648
    }

-- Generated for mainnet, first address of an additional account.
decodeTest2 :: DecodeDerivationPath
decodeTest2 = DecodeDerivationPath
    { mnem = addrMnemonic
    , addr = "DdzFFzCqrht2qSNuod2j3HQdxQYu7ehMnHqPMK6ZCZc1oTBfFJFTaqMF62rzWsJWZhbrN15uA4Bsp6M7t5WkqfumdnjLjZ5xRk8szuCd"
    , accIndex = 2694138340
    , addrIndex = 2512821145
    }

decodeTest3 :: DecodeDerivationPath
decodeTest3 = DecodeDerivationPath
    { mnem = addrMnemonic
    , addr = "37btjrVyb4KEFr9tdBDYVPUWpFAu65yfnoqWmd5aeZpBk7MKTyH5tiPZ7sFi6k4vWXS5Df7H7Z4CT4m3uJ1Ps4ck7rrzqWDmtiifXoqX2MQHSGYeon"
    , accIndex = 2147483648
    , addrIndex = 2147483648
    }

decodeTest4 :: DecodeDerivationPath
decodeTest4 = DecodeDerivationPath
    { mnem = addrMnemonic
    , addr = "37btjrVyb4KBbK7wGESZtW3vXSj8c8fGGHwS2b2fnCd6erjPw3Nt2Nw4RLbrYbdLbdgseiF3YaawsT1JGes9FMrW6Fuye9ANyLhkTq2EiHsnPE4qso"
    , accIndex = 3337448281
    , addrIndex = 3234874775
    }

-- | Random empty wallet. It's not possible to restore a wallet from
-- 'defMnemonic', so that's why there are two mnemonics in these tests.
addrMnemonic :: [Text]
addrMnemonic =
    [ "price", "whip", "bottom", "execute", "resist", "library"
    , "entire", "purse", "assist", "clock", "still", "noble" ]

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

-- | Get a private key from a hex string, without error checking.
xprv16 :: ByteString -> RndKey 'RootK XPrv
xprv16 hex = RndKey k () (error "passphrase not used for tests")
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
