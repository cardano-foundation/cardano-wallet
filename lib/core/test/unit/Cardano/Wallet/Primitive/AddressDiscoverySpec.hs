{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDiscoverySpec
    ( spec
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( ChainCode (..), XPrv, XPub (..), unXPrv )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , Depth (AccountK, AddressK, RootK)
    , DerivationType (Hardened)
    , Index
    , PaymentAddress (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , networkDiscriminantVal
    , passphraseMaxLength
    , passphraseMinLength
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey
    , generateKeyFromSeed
    , minSeedLengthBytes
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( DecodeAddress (..), EncodeAddress (..), IsOurs (..), knownAddresses )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( mkRndState )
import Cardano.Wallet.Primitive.Types
    ( Address (..), ShowFmt (..) )
import Control.Monad
    ( replicateM )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..) )
import Data.Word
    ( Word8 )
import Test.Hspec
    ( Spec, SpecWith, describe, expectationFailure, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , Property
    , arbitraryBoundedEnum
    , arbitraryPrintableChar
    , choose
    , frequency
    , oneof
    , property
    , vectorOf
    , withMaxSuccess
    , (.&&.)
    , (===)
    )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "encodeAddress & decodeAddress (Mainnet)" $ do
        let proxy = Proxy @'Mainnet
        it "decodeAddress . encodeAddress = pure" $
            withMaxSuccess 1000 $ property $ \(ShowFmt a, _ :: Proxy 'Mainnet) ->
                (ShowFmt <$> decodeAddress @'Mainnet (encodeAddress @'Mainnet a))
                    === Right (ShowFmt a)
        negativeTest proxy "ta1sdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677ztw225s"
            ("This Address belongs to another network. Network is: "
            <> show (networkDiscriminantVal @'Mainnet) <> ".")
        negativeTest proxy "EkxDbkPo"
            "Unable to decode Address: neither Bech32-encoded nor a valid Byron \
            \Address."
        negativeTest proxy ".%14'"
            ("Unable to decode Address: encoding is neither Bech32 nor Base58.")
        negativeTest proxy "ca1qvqsyqcyq5rqwzqfpg9scrgk66qs0"
            "Invalid Address length (14): expected either 33 or 65 bytes."
        negativeTest proxy
            "ca1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqscdket"
            ("Invalid Address first byte.")
        negativeTest proxy
            "ca1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqqgzqvz\
            \q2ps8pqys5zcvp58q7yq3zgf3g9gkzuvpjxsmrsw3u8eqwxpnc0"
            ("Invalid Address first byte.")
        -- NOTE:
        -- Data below have been generated with [jcli](https://github.com/input-output-hk/jormungandr/tree/master/doc/jcli)
        -- as described in the annex at the end of the file.
        goldenTestAddr proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            ]
            "ca1qdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677zqx4le2"
        goldenTestAddr proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            ]
            "ca1q00e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr78edvht"
        goldenTestAddr proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            , "b24e70b0c2ceeb24cc9f28f386478c73aa71c05a95a0119bb91dd8e89c3592ae"
            ]
            "ca1q3aa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677r\
            \vjwwzcv9nhtynxf728nserccua2w8q949dqzxdmj8wcazwrty4wga8haz"
        goldenTestAddr proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            , "402abff6065c847115ad22ff6b0d3a85fd69a6fcc32ed76aa8cadb305b0c51a7"
            ]
            "ca1qn0e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr7\
            \sp2hlmqvhyywy266ghldvxn4p0adxn0esew6a423jkmxpdsc5d8hxd7cr"

    describe "encode & decodeAddress (Testnet)" $ do
        let proxy = Proxy @'Testnet
        it "decodeAddress . encodeAddress = pure" $
            withMaxSuccess 1000 $ property $ \(ShowFmt a, _ :: Proxy 'Testnet) ->
                (ShowFmt <$> decodeAddress @'Testnet (encodeAddress @'Testnet a))
                    === Right (ShowFmt a)
        negativeTest proxy "ca1qdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677zqx4le2"
            ("This Address belongs to another network. Network is: "
            <> show (networkDiscriminantVal @'Testnet) <> ".")
        negativeTest proxy "EkxDbkPo"
            "Unable to decode Address: neither Bech32-encoded nor a valid Byron \
            \Address."
        negativeTest proxy ".%14'"
            ("Unable to decode Address: encoding is neither Bech32 nor Base58.")
        negativeTest proxy "ta1dvqsyqcyq5rqwzqfpg9scrg5v76st"
            "Invalid Address length (14): expected either 33 or 65 bytes."
        negativeTest proxy
            "ta1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jq8ygppa"
            ("Invalid Address first byte.")
        negativeTest proxy
            "ta1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqqgzqvz\
            \q2ps8pqys5zcvp58q7yq3zgf3g9gkzuvpjxsmrsw3u8eq9lcgc2"
            ("Invalid Address first byte.")
        goldenTestAddr proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            ]
            "ta1sdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677ztw225s"
        goldenTestAddr proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            ]
            "ta1s00e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr7v3je63"
        goldenTestAddr proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            , "b24e70b0c2ceeb24cc9f28f386478c73aa71c05a95a0119bb91dd8e89c3592ae"
            ]
            "ta1s3aa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677r\
            \vjwwzcv9nhtynxf728nserccua2w8q949dqzxdmj8wcazwrty4we4spcz"
        goldenTestAddr proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            , "402abff6065c847115ad22ff6b0d3a85fd69a6fcc32ed76aa8cadb305b0c51a7"
            ]
            "ta1sn0e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr7\
            \sp2hlmqvhyywy266ghldvxn4p0adxn0esew6a423jkmxpdsc5d8xw6gar"

    describe "Random Address Discovery Properties" $ do
        it "isOurs works as expected during key derivation in testnet" $ do
            property (prop_derivedKeysAreOurs @'Testnet)
        it "isOurs works as expected during key derivation in mainnet" $ do
            property (prop_derivedKeysAreOurs @'Mainnet)

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_derivedKeysAreOurs
    :: forall (n :: NetworkDiscriminant). (PaymentAddress n ByronKey)
    => Passphrase "seed"
    -> Passphrase "encryption"
    -> Index 'Hardened 'AccountK
    -> Index 'Hardened 'AddressK
    -> ByronKey 'RootK XPrv
    -> Property
prop_derivedKeysAreOurs seed encPwd accIx addrIx rk' =
    resPos .&&. addr `elem` knownAddresses stPos' .&&.
    not resNeg .&&. addr `notElem` knownAddresses stNeg'
  where
    (resPos, stPos') = isOurs addr (mkRndState @n rootXPrv 0)
    (resNeg, stNeg') = isOurs addr (mkRndState @n rk' 0)
    key = publicKey $ unsafeGenerateKeyFromSeed (accIx, addrIx) seed encPwd
    rootXPrv = generateKeyFromSeed seed encPwd
    addr = paymentAddress @n key


negativeTest
    :: forall n. DecodeAddress n
    => Proxy n
    -> Text
    -> String
    -> SpecWith ()
negativeTest _proxy input msg = it ("decodeAddress failure: " <> msg) $
    decodeAddress @n input === Left (TextDecodingError msg)

-- | Generate addresses from the given keys and compare the result with an
-- expected output obtained from jcli (see appendix below)
goldenTestAddr
    :: forall n. (PaymentAddress n ShelleyKey, DelegationAddress n ShelleyKey, EncodeAddress n)
    => Proxy n
    -> [ByteString]
    -> Text
    -> SpecWith ()
goldenTestAddr _proxy pubkeys expected = it ("golden test: " <> T.unpack expected) $ do
    case traverse (convertFromBase Base16) pubkeys of
        Right [spending] -> do
            let xpub = ShelleyKey (XPub spending chainCode)
            let addr = encodeAddress @n (paymentAddress @n xpub)
            addr `shouldBe` expected
        Right [spending, delegation] -> do
            let xpubSpending = ShelleyKey (XPub spending chainCode)
            let xpubDeleg = ShelleyKey (XPub delegation chainCode)
            let addr = encodeAddress @n (delegationAddress @n xpubSpending xpubDeleg)
            addr `shouldBe` expected
        _ ->
            expectationFailure "goldenTestAddr: provided invalid inputs public keys"
  where
    chainCode = ChainCode "<ChainCode is not used by singleAddressFromKey>"

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Index 'Hardened 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (ByronKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = genRootKeys

genRootKeys :: Gen (ByronKey 'RootK XPrv)
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

instance Show XPrv where
    show = show . unXPrv

instance {-# OVERLAPS #-} Arbitrary (Passphrase "encryption") where
    arbitrary = do
        let p = Proxy :: Proxy "encryption"
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes

instance {-# OVERLAPS #-} Arbitrary (Passphrase "seed") where
    arbitrary = do
        n <- choose (minSeedLengthBytes, 64)
        bytes <- BS.pack <$> vectorOf n arbitrary
        return $ Passphrase $ BA.convert bytes

instance {-# OVERLAPS #-} Arbitrary (ShowFmt Address, Proxy 'Testnet) where
    arbitrary = do
        let proxy = Proxy @'Testnet
        addr <- ShowFmt <$> frequency
            [ (10, genAddress 0x83 0x84)
            , (1, genLegacyAddress (30, 100))
            ]
        return (addr, proxy)

instance {-# OVERLAPS #-} Arbitrary (ShowFmt Address, Proxy 'Mainnet) where
    arbitrary = do
        let proxy = Proxy @'Mainnet
        addr <- ShowFmt <$> frequency
            [ (10, genAddress 0x03 0x04)
            , (1, genLegacyAddress (30, 100))
            ]
        return (addr, proxy)

genAddress :: Word8 -> Word8 -> Gen Address
genAddress single grouped = oneof
    [ (\bytes -> Address (BS.pack (single:bytes))) <$> vectorOf 32 arbitrary
    , (\bytes -> Address (BS.pack (grouped:bytes))) <$> vectorOf 64 arbitrary
    ]

genLegacyAddress :: (Int, Int) -> Gen Address
genLegacyAddress range = do
    n <- choose range
    let prefix = BS.pack
            [ 130       -- Array(2)
            , 216, 24   -- Tag 24
            , 88, fromIntegral n -- Bytes(n), n > 23 && n < 256
            ]
    payload <- BS.pack <$> vectorOf n arbitrary
    let crc = BS.pack [26,1,2,3,4]
    return $ Address (prefix <> payload <> crc)

{-------------------------------------------------------------------------------
            Generating Golden Test Vectors For Address Encoding
-------------------------------------------------------------------------------}

-- SPENDINGKEY=$(jcli key generate --type Ed25519Extended | jcli key to-public)
-- DELEGATIONKEY=$(jcli key generate --type Ed25519Extended | jcli key to-public)
--
-- SPENDINGKEYBYTES=$(echo $SPENDINGKEY | jcli key to-bytes)
-- DELEGATIONKEYBYTES=$(echo $DELEGATIONKEY | jcli key to-bytes)
--
-- MAINNETSINGLE=$(jcli address single $SPENDINGKEY)
-- TESTNETSINGLE=$(jcli address single $SPENDINGKEY --testing)
--
-- MAINNETGROUPED=$(jcli address single $SPENDINGKEY $DELEGATIONKEY)
-- TESTNETGROUPED=$(jcli address single $SPENDINGKEY $DELEGATIONKEY --testing)
--
-- TESTVECTOR=test_vector_$(date +%s)
-- touch $TESTVECTOR
-- echo "spending key:        $SPENDINGKEYBYTES" >> $TESTVECTOR
-- echo "\ndelegation key:    $DELEGATIONKEYBYTES" >> $TESTVECTOR
-- echo "\nsingle (mainnet):  $MAINNETSINGLE" >> $TESTVECTOR
-- echo "\ngrouped (mainnet): $MAINNETGROUPED" >> $TESTVECTOR
-- echo "\nsingle (testnet):  $TESTNETSINGLE" >> $TESTVECTOR
-- echo "\ngrouped (testnet): $TESTNETGROUPED" >> $TESTVECTOR
--
-- echo -e $(cat $TESTVECTOR)
-- echo "Saved as $TESTVECTOR."
