{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.HttpBridge.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( unXPrv )
import Cardano.Wallet.HttpBridge.Binary
    ( decodeAddressDerivationPath )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, Network (..) )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..) )
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
import Cardano.Wallet.Primitive.AddressDerivation
    ( fromMnemonic )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey (..)
    , addrToPayload
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , deserialise
    , generateKeyFromSeed
    , minSeedLengthBytes
    )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( unsafeGenerateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), decodeAddress, encodeAddress )
import Control.Monad
    ( replicateM )
import Data.ByteString
    ( ByteString )
import Data.Digest.CRC32
    ( crc32 )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..) )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Expectation, Spec, describe, it, shouldBe )
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
import qualified Data.ByteString.Char8 as B8
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
        it "Key derivation works for various indexes" $
            property prop_keyDerivation
        it "isOurs works as expected during key derivation in testnet" $ do
            property (prop_derivedKeysAreOurs @'Testnet)
        it "isOurs works as expected during key derivation in mainnet" $ do
            property (prop_derivedKeysAreOurs @'Mainnet)

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

    it "decoding works - mainnet - initial account" $
        decodeTest decodeTest1

    it "decoding works - mainnet - another account" $
        decodeTest decodeTest2

    it "decoding works - testnet - initial account" $
        decodeTest decodeTest3

    it "decoding works - testnet - another account" $
        decodeTest decodeTest4


decodeTest :: DecodeDerivationPath -> Expectation
decodeTest DecodeDerivationPath{..} =
    decoded `shouldBe` Right (Just (Index accIndex, Index addrIndex))
  where
    decoded = deserialise (decodeAddressDerivationPath rootXPub) (addrToPayload $ Address addr)
    rootXPub = payloadPassphrase $ publicKey $ generateKeyFromSeed (Passphrase seed) (Passphrase "")
    Right (Passphrase seed) = fromMnemonic @'[12] mnem


checkIsOurs :: DecodeDerivationPath -> Expectation
checkIsOurs DecodeDerivationPath{..} =
    isOurs (Address addr) (RndState rootXPrv) `shouldBe` (True, RndState rootXPrv)
  where
    rootXPrv = generateKeyFromSeed (Passphrase seed) (Passphrase "")
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

data DecodeDerivationPath = DecodeDerivationPath
    { mnem :: [Text]
    , addr :: ByteString
    , accIndex :: Word32
    , addrIndex :: Word32
    } deriving (Generic, Show, Eq)



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

-- All keys derived from a given seed should satisfy IsOurs with the same key.
prop_derivedKeysAreOurs
    :: forall (n :: Network).
       (KnownNetwork n, KeyToAddress (HttpBridge n) RndKey)
    => Passphrase "seed"
    -> Passphrase "encryption"
    -> Index 'Hardened 'AccountK
    -> Index 'Hardened 'AddressK
    -> RndKey 'RootK XPrv
    -> Property
prop_derivedKeysAreOurs seed encPwd accIx addrIx rk' =
    isOurs address (RndState rootXPrv) === (True, RndState rootXPrv) .&&.
    isOurs address (RndState rk') === (False, RndState rk')
  where
    rootXPrv = generateKeyFromSeed seed encPwd
    accXPrv = deriveAccountPrivateKey encPwd rootXPrv accIx
    addrXPrv = deriveAddressPrivateKey encPwd accXPrv addrIx
    addrXPub = publicKey addrXPrv
    address = keyToAddress @(HttpBridge n) addrXPub


{-------------------------------------------------------------------------------
                    Instances
-------------------------------------------------------------------------------}

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

-- Generate only valid seeds. The precondition of 'generateKey' is that the seed
-- is of sufficient length, and it crashes otherwise
instance {-# OVERLAPS #-} Arbitrary (Passphrase "seed") where
    arbitrary = do
        -- fixme: why only printable chars? easier to print?
        chars <- replicateM minSeedLengthBytes arbitraryPrintableChar
        return $ Passphrase $ BA.convert $ B8.pack chars

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
