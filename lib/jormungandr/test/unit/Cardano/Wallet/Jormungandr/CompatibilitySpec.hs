{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), DecodeAddress (..), EncodeAddress (..), ShowFmt (..) )
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
import Test.Hspec
    ( Spec, SpecWith, describe, expectationFailure, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , choose
    , frequency
    , oneof
    , property
    , vectorOf
    , withMaxSuccess
    , (===)
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

spec :: Spec
spec = describe "EncodeAddress & DecodeAddress" $ do
    describe "Mainnet" $ do
        let proxy = Proxy @(Jormungandr 'Mainnet)
        let firstByteS = B8.unpack (BS.pack [single @'Mainnet])
        let firstByteG = B8.unpack (BS.pack [grouped @'Mainnet])

        it "decodeAddress . encodeAddress = pure" $
            withMaxSuccess 1000 $ property $ \(ShowFmt a, _ :: Proxy 'Mainnet) ->
                (ShowFmt <$> decodeAddress proxy (encodeAddress proxy a))
                    === Right (ShowFmt a)
        negativeTest proxy "bc1qvqsyqcyq5rqwzqfpg9scrgyg0p0q"
            ("This address belongs to another network. Network is: "
            <> show (networkVal @'Mainnet) <> ".")
        negativeTest proxy "EkxDbkPo"
            "Unable to decode address: neither Bech32-encoded nor a valid Byron \
            \address."
        negativeTest proxy ".%14'"
            ("Unable to decode address: encoding is neither Bech32 nor Base58.")
        negativeTest proxy "ca1qvqsyqcyq5rqwzqfpg9scrgk66qs0"
            "Invalid address length (14): expected either 33 or 65 bytes."
        negativeTest proxy
            "ca1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqscdket"
            ("Invalid address first byte: k =/= " <> firstByteS <> ".")
        negativeTest proxy
            "ca1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqqgzqvz\
            \q2ps8pqys5zcvp58q7yq3zgf3g9gkzuvpjxsmrsw3u8eqwxpnc0"
            ("Invalid address first byte: k =/= " <> firstByteG <> ".")
        -- NOTE:
        -- Data below have been generated with [jcli](https://github.com/input-output-hk/jormungandr/tree/master/doc/jcli)
        -- as described in the annex at the end of the file.
        goldenTest proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            ]
            "ca1qdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677zqx4le2"
        goldenTest proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            ]
            "ca1q00e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr78edvht"
        goldenTest proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            , "b24e70b0c2ceeb24cc9f28f386478c73aa71c05a95a0119bb91dd8e89c3592ae"
            ]
            "ca1q3aa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677r\
            \vjwwzcv9nhtynxf728nserccua2w8q949dqzxdmj8wcazwrty4wga8haz"
        goldenTest proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            , "402abff6065c847115ad22ff6b0d3a85fd69a6fcc32ed76aa8cadb305b0c51a7"
            ]
            "ca1qn0e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr7\
            \sp2hlmqvhyywy266ghldvxn4p0adxn0esew6a423jkmxpdsc5d8hxd7cr"

    describe "Testnet" $ do
        let proxy = Proxy @(Jormungandr 'Testnet)
        let firstByteS = B8.unpack (BS.pack [single @'Testnet])
        let firstByteG = B8.unpack (BS.pack [grouped @'Testnet])

        it "decodeAddress . encodeAddress = pure" $
            withMaxSuccess 1000 $ property $ \(ShowFmt a, _ :: Proxy 'Testnet) ->
                (ShowFmt <$> decodeAddress proxy (encodeAddress proxy a))
                    === Right (ShowFmt a)
        negativeTest proxy "bc1qvqsyqcyq5rqwzqfpg9scrgyg0p0q"
            ("This address belongs to another network. Network is: "
            <> show (networkVal @'Testnet) <> ".")
        negativeTest proxy "EkxDbkPo"
            "Unable to decode address: neither Bech32-encoded nor a valid Byron \
            \address."
        negativeTest proxy ".%14'"
            ("Unable to decode address: encoding is neither Bech32 nor Base58.")
        negativeTest proxy "ta1dvqsyqcyq5rqwzqfpg9scrg5v76st"
            "Invalid address length (14): expected either 33 or 65 bytes."
        negativeTest proxy
            "ta1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jq8ygppa"
            ("Invalid address first byte: k =/= " <> firstByteS <> ".")
        negativeTest proxy
            "ta1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqqgzqvz\
            \q2ps8pqys5zcvp58q7yq3zgf3g9gkzuvpjxsmrsw3u8eq9lcgc2"
            ("Invalid address first byte: k =/= " <> firstByteG <> ".")
        goldenTest proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            ]
            "ta1sdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677ztw225s"
        goldenTest proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            ]
            "ta1s00e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr7v3je63"
        goldenTest proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            , "b24e70b0c2ceeb24cc9f28f386478c73aa71c05a95a0119bb91dd8e89c3592ae"
            ]
            "ta1s3aa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677r\
            \vjwwzcv9nhtynxf728nserccua2w8q949dqzxdmj8wcazwrty4we4spcz"
        goldenTest proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            , "402abff6065c847115ad22ff6b0d3a85fd69a6fcc32ed76aa8cadb305b0c51a7"
            ]
            "ta1sn0e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr7\
            \sp2hlmqvhyywy266ghldvxn4p0adxn0esew6a423jkmxpdsc5d8xw6gar"

negativeTest
    :: DecodeAddress t
    => Proxy t
    -> Text
    -> String
    -> SpecWith ()
negativeTest proxy input msg = it ("decodeAddress failure: " <> msg) $
    decodeAddress proxy input === Left (TextDecodingError msg)

-- | Generate addresses from the given keys and compare the result with an
-- expected output.
goldenTest
    :: forall t n. (t ~ Jormungandr n, EncodeAddress t, KnownNetwork n)
    => Proxy t
    -> [ByteString]
    -> Text
    -> SpecWith ()
goldenTest proxy pubkeys expected = it ("golden test: " <> T.unpack expected) $ do
    case traverse (convertFromBase Base16) pubkeys of
        Right [spending] -> do
            let payload = BS.pack [single @n] <> spending
            let addr = encodeAddress proxy (Address payload)
            addr `shouldBe` expected
        Right [spending, delegation] -> do
            let payload = BS.pack [grouped @n] <> spending <> delegation
            let addr = encodeAddress proxy (Address payload)
            addr `shouldBe` expected
        _ ->
            expectationFailure "goldenTest: provided invalid inputs public keys"

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance {-# OVERLAPS #-} KnownNetwork n => Arbitrary (ShowFmt Address, Proxy n) where
    arbitrary = do
        let proxy = Proxy @n
        addr <- ShowFmt <$> frequency
            [ (10, genAddress proxy)
            , (1, genLegacyAddress (30, 100))
            ]
        return (addr, proxy)

genAddress :: forall n. KnownNetwork n => Proxy n -> Gen Address
genAddress _ = oneof
    [ (\bytes -> Address (BS.pack ((single @n):bytes))) <$> vectorOf 32 arbitrary
    , (\bytes -> Address (BS.pack ((grouped @n):bytes))) <$> vectorOf 64 arbitrary
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
                        Generating Golden Test Vectors
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
