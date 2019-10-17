{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.CompatibilitySpec
    ( spec
    )where

import Prelude

import Cardano.Crypto.Wallet
    ( ChainCode (..), XPrv, XPub (..), unXPrv )
import Cardano.Wallet.Jormungandr.Binary
    ( signData, singleAddressFromKey )
import Cardano.Wallet.Jormungandr.Compatibility
    ( BaseUrl (..), Jormungandr, Scheme (..), genConfigFile )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (AccountK, AddressK, RootK)
    , DerivationType (Hardened)
    , Index
    , KeyToAddress (..)
    , Passphrase (..)
    , passphraseMaxLength
    , passphraseMinLength
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey
    , generateKeyFromSeed
    , minSeedLengthBytes
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs (..), knownAddresses )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( mkRndState )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , DecodeAddress (..)
    , EncodeAddress (..)
    , Hash (..)
    , ShowFmt (..)
    , TxIn (..)
    , TxOut (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeDecodeAddress, unsafeFromHex )
import Control.Monad
    ( replicateM )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
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
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "signData @(Jormungandr 'Mainnet)" $ do
        let proxy = Proxy @(Jormungandr 'Mainnet)
        let fakeIn0 = Hash $ unsafeFromHex
                "666984dec4bc0ff1888be97bfe0694a96b35c58d025405ead51d5cc72a3019f4"
        let fakeIn1 = Hash $ unsafeFromHex
                "1323856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1"
        let addr0 = unsafeDecodeAddress proxy
                "ca1qw0rtl2hpm6zlj4vsjkjyrtk7x2ujclrx8wwxakh4yh7s2xegalpxyayx7e"
        let addr1 = unsafeDecodeAddress proxy
                "ca1qv474k2hgzr5h68v5khvfv6p95mac6wlkztw9azwe73casfk420ykxsjume"
        let change = unsafeDecodeAddress proxy
                "ca1q0nh79pzqxge7qy8jp2zu9rdxqsepe55qlw756zzuy8hgddqvru95l5syeu"
        goldenTestSignData proxy (GoldenTestSignData
            { gtInputs =
                [ (TxIn fakeIn0 1, Coin 14442)
                ]
            , gtOutputs =
                [ TxOut addr0 (Coin 1337)
                , TxOut change (Coin 13105)
                ]
            , gtExpected =
                "7bfbf66a97e0b4a96fa27c8fb80cb20f89280debf259b1a6b7e4d4b9e8c7c552"
            })
        goldenTestSignData proxy (GoldenTestSignData
            { gtInputs =
                [ (TxIn fakeIn0 1, Coin 14442)
                ]
            , gtOutputs =
                [ TxOut addr0 (Coin 1337)
                ]
            , gtExpected =
                "0be6fdb0c1d9c5c2493a319bcc659e97d1b04fcad002878e186f46852b6cf208"
            })
        goldenTestSignData proxy (GoldenTestSignData
            { gtInputs =
                [ (TxIn fakeIn0 1, Coin 14442)
                , (TxIn fakeIn1 1, Coin 42)
                , (TxIn fakeIn0 2, Coin 123456789)
                ]
            , gtOutputs =
                [ TxOut addr0 (Coin 14)
                , TxOut addr1 (Coin 123456)
                ]
            , gtExpected =
                "115ba47ce89d8d92c5ed6b5c11be57ee5c757848f65a350eb49adff8f625b7a6"
            })

    describe "signData @(Jormungandr 'Testnet)" $ do
        let proxy = Proxy @(Jormungandr 'Testnet)
        let fakeIn0 = Hash $ unsafeFromHex
                "666984dec4bc0ff1888be97bfe0694a96b35c58d025405ead51d5cc72a3019f4"
        let fakeIn1 = Hash $ unsafeFromHex
                "1323856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1"
        let addr0 = unsafeDecodeAddress proxy
                "ta1s0uz3rda4xx66fttdfxhdz9kh0pd5p9llhgeslmfll227yy2l9s66mhf7pv"
        let addr1 = unsafeDecodeAddress proxy
                "ta1s09dcqava9nsl889fyrhuufx0reavt76qa803rzmllfur7cxcmt2xqsxwsm"
        let change = unsafeDecodeAddress proxy
                "ta1sw4rdkek9rc5ywhrfna92wulm7ljdr5000g8kysz2hf2hgeyl5h755fhef2"
        goldenTestSignData proxy (GoldenTestSignData
            { gtInputs =
                [ (TxIn fakeIn0 1, Coin 14442)
                ]
            , gtOutputs =
                [ TxOut addr0 (Coin 1337)
                , TxOut change (Coin 13105)
                ]
            , gtExpected =
                "4c9097d2121d0a7f10c9c04d25a310999d36cf07ac232fd0e72feb1ea8e93dc7"
            })
        goldenTestSignData proxy (GoldenTestSignData
            { gtInputs =
                [ (TxIn fakeIn0 1, Coin 14442)
                ]
            , gtOutputs =
                [ TxOut addr0 (Coin 1337)
                ]
            , gtExpected =
                "1de8b9655106fe0b8c2e5952a9f105d518838296b79d542f4e96b5980ed2b909"
            })
        goldenTestSignData proxy (GoldenTestSignData
            { gtInputs =
                [ (TxIn fakeIn0 1, Coin 14442)
                , (TxIn fakeIn1 1, Coin 42)
                , (TxIn fakeIn0 2, Coin 123456789)
                ]
            , gtOutputs =
                [ TxOut addr0 (Coin 14)
                , TxOut addr1 (Coin 123456)
                ]
            , gtExpected =
                "0ba2569f273b5bbb2cf37a749dab8416cf1574cc22b7757953d46d8a979657da"
            })

    describe "encodeAddress & decodeAddress (Mainnet)" $ do
        let proxy = Proxy @(Jormungandr 'Mainnet)
        let firstByteS = B8.unpack (BS.pack [single @'Mainnet])
        let firstByteG = B8.unpack (BS.pack [grouped @'Mainnet])

        it "decodeAddress . encodeAddress = pure" $
            withMaxSuccess 1000 $ property $ \(ShowFmt a, _ :: Proxy 'Mainnet) ->
                (ShowFmt <$> decodeAddress proxy (encodeAddress proxy a))
                    === Right (ShowFmt a)
        negativeTest proxy "bc1qvqsyqcyq5rqwzqfpg9scrgyg0p0q"
            ("This Address belongs to another network. Network is: "
            <> show (networkVal @'Mainnet) <> ".")
        negativeTest proxy "EkxDbkPo"
            "Unable to decode Address: neither Bech32-encoded nor a valid Byron \
            \Address."
        negativeTest proxy ".%14'"
            ("Unable to decode Address: encoding is neither Bech32 nor Base58.")
        negativeTest proxy "ca1qvqsyqcyq5rqwzqfpg9scrgk66qs0"
            "Invalid Address length (14): expected either 33 or 65 bytes."
        negativeTest proxy
            "ca1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqscdket"
            ("Invalid Address first byte: k =/= " <> firstByteS <> ".")
        negativeTest proxy
            "ca1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqqgzqvz\
            \q2ps8pqys5zcvp58q7yq3zgf3g9gkzuvpjxsmrsw3u8eqwxpnc0"
            ("Invalid Address first byte: k =/= " <> firstByteG <> ".")
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
        let proxy = Proxy @(Jormungandr 'Testnet)
        let firstByteS = B8.unpack (BS.pack [single @'Testnet])
        let firstByteG = B8.unpack (BS.pack [grouped @'Testnet])

        it "decodeAddress . encodeAddress = pure" $
            withMaxSuccess 1000 $ property $ \(ShowFmt a, _ :: Proxy 'Testnet) ->
                (ShowFmt <$> decodeAddress proxy (encodeAddress proxy a))
                    === Right (ShowFmt a)
        negativeTest proxy "bc1qvqsyqcyq5rqwzqfpg9scrgyg0p0q"
            ("This Address belongs to another network. Network is: "
            <> show (networkVal @'Testnet) <> ".")
        negativeTest proxy "EkxDbkPo"
            "Unable to decode Address: neither Bech32-encoded nor a valid Byron \
            \Address."
        negativeTest proxy ".%14'"
            ("Unable to decode Address: encoding is neither Bech32 nor Base58.")
        negativeTest proxy "ta1dvqsyqcyq5rqwzqfpg9scrg5v76st"
            "Invalid Address length (14): expected either 33 or 65 bytes."
        negativeTest proxy
            "ta1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jq8ygppa"
            ("Invalid Address first byte: k =/= " <> firstByteS <> ".")
        negativeTest proxy
            "ta1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqqgzqvz\
            \q2ps8pqys5zcvp58q7yq3zgf3g9gkzuvpjxsmrsw3u8eq9lcgc2"
            ("Invalid Address first byte: k =/= " <> firstByteG <> ".")
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

    describe "genConfigFile" $ do
        it "example configuration" $ do
            let stateDir = "/state-dir"
            let baseUrl = BaseUrl Http "127.0.0.1" 8080 "/api"
            let user = [aesonQQ|{
                "yolo": true,
                "p2p": {
                    "trusted_peers": [{
                        "address": "/ip4/208.80.152.201/tcp/3000",
                        "id": "hello"
                     }]
                 }
            }|]
            let expected = [aesonQQ|{
                "storage": "/state-dir/chain",
                "rest": {
                    "listen": "127.0.0.1:8080"
                },
                "p2p": {
                    "trusted_peers": [{
                        "address": "/ip4/208.80.152.201/tcp/3000",
                        "id": "hello"
                     }],
                    "topics_of_interest": {
                        "messages": "low",
                        "blocks": "normal"
                    },
                    "public_address" : "/ip4/127.0.0.1/tcp/8081"
                },
                "yolo": true
            }|]

            genConfigFile stateDir 8081 baseUrl user `shouldBe` expected

    describe "Random Address Discovery Properties" $ do
        it "isOurs works as expected during key derivation in testnet" $ do
            property (prop_derivedKeysAreOurs @'Testnet)
        it "isOurs works as expected during key derivation in mainnet" $ do
            property (prop_derivedKeysAreOurs @'Mainnet)

negativeTest
    :: DecodeAddress t
    => Proxy t
    -> Text
    -> String
    -> SpecWith ()
negativeTest proxy input msg = it ("decodeAddress failure: " <> msg) $
    decodeAddress proxy input === Left (TextDecodingError msg)

-- | Generate addresses from the given keys and compare the result with an
-- expected output obtained from jcli (see appendix below)
goldenTestAddr
    :: forall t n. (t ~ Jormungandr n, EncodeAddress t, KnownNetwork n)
    => Proxy t
    -> [ByteString]
    -> Text
    -> SpecWith ()
goldenTestAddr proxy pubkeys expected = it ("golden test: " <> T.unpack expected) $ do
    case traverse (convertFromBase Base16) pubkeys of
        Right [spending] -> do
            let xpub = XPub spending chainCode
            let rawAddr = singleAddressFromKey (Proxy @n) xpub
            let addr = encodeAddress proxy rawAddr
            addr `shouldBe` expected
        Right [spending, delegation] -> do
            let payload = BS.pack [grouped @n] <> spending <> delegation
            let addr = encodeAddress proxy (Address payload)
            addr `shouldBe` expected
        _ ->
            expectationFailure "goldenTestAddr: provided invalid inputs public keys"
  where
    chainCode = ChainCode "<ChainCode is not used by singleAddressFromKey>"

data GoldenTestSignData = GoldenTestSignData
    { gtInputs :: [(TxIn, Coin)]
    , gtOutputs :: [TxOut]
    , gtExpected :: ByteString
    }

-- | Generate tx ids for the given transaction and compare the result with an
-- expected output obtained from jcli (see appendix below)
--
-- Note that jcli doesn't give the fragment id but, it gives the signing data
-- (so the tx id as we knew it).
goldenTestSignData
    :: forall t n. (t ~ Jormungandr n)
    => Proxy (Jormungandr n)
    -> GoldenTestSignData
    -> SpecWith ()
goldenTestSignData _ (GoldenTestSignData ins outs expected) =
    it ("golden test: " <> B8.unpack expected) $
        hex (getHash $ signData ins outs) `shouldBe` expected
  where
    hex = convertToBase @ByteString @ByteString Base16

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_derivedKeysAreOurs
    :: forall (n :: Network) t. (KeyToAddress t RndKey, t ~ Jormungandr n)
    => Passphrase "seed"
    -> Passphrase "encryption"
    -> Index 'Hardened 'AccountK
    -> Index 'Hardened 'AddressK
    -> RndKey 'RootK XPrv
    -> Property
prop_derivedKeysAreOurs seed encPwd accIx addrIx rk' =
    resPos .&&. addr `elem` knownAddresses stPos' .&&.
    not resNeg .&&. addr `notElem` knownAddresses stNeg'
  where
    (resPos, stPos') = isOurs addr (mkRndState @t rootXPrv 0)
    (resNeg, stNeg') = isOurs addr (mkRndState @t rk' 0)
    key = publicKey $ unsafeGenerateKeyFromSeed (accIx, addrIx) seed encPwd
    rootXPrv = generateKeyFromSeed seed encPwd
    addr = keyToAddress @t key

{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (Index 'Hardened 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

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

{-------------------------------------------------------------------------------
            Generating Golden Test Vectors For TxId
-------------------------------------------------------------------------------}

-- NETWORK=${NETWORK:-testnet}
-- case $NETWORK in
--   'mainnet')
--     DISCRIMINATION=""
--     ;;
--   'testnet')
--     DISCRIMINATION="--testing"
--     ;;
--   *)
--     echo "Unknown network: $NETWORK"
--     exit 1
-- esac
--
-- # Dummy Data
-- BLOCK0=13c3d835c53a198f7c8513b04d99eeb23c745c0a73364c2f0e802fa38eec9dba
-- FAKEIN0=666984dec4bc0ff1888be97bfe0694a96b35c58d025405ead51d5cc72a3019f4
-- FAKEIN1=1323856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1
--
-- # Generate some keys
-- ROOTPRV0=$(jcli key generate --type ed25519extended)
-- ROOTPUB0=$(echo $ROOTPRV0 | jcli key to-public)
-- ADDR0=$(jcli address single $DISCRIMINATION $ROOTPUB0)
-- echo "Addr 0: $ADDR0"
--
-- ROOTPRV1=$(jcli key generate --type ed25519extended)
-- ROOTPUB1=$(echo $ROOTPRV1 | jcli key to-public)
-- ADDR1=$(jcli address single $DISCRIMINATION $ROOTPUB1)
-- echo "Addr 1: $ADDR1"
--
-- ROOTPRV2=$(jcli key generate --type ed25519extended)
-- ROOTPUB2=$(echo $ROOTPRV2 | jcli key to-public)
-- CHANGE=$(jcli address single $DISCRIMINATION $ROOTPUB2)
-- echo "Change: $CHANGE"
--
-- echo ""
--
-- # One input, one output, one change
-- echo $ROOTPRV0 | jcli transaction make-witness $FAKEIN0 --genesis-block-hash $BLOCK0 --type utxo > /tmp/wit
-- TX=$(jcli transaction new \
--   | jcli transaction add-input $FAKEIN0 1 14442 \
--   | jcli transaction add-output $ADDR0 1337 \
--   | jcli transaction finalize $CHANGE \
--   | jcli transaction add-witness /tmp/wit \
--   | jcli transaction seal \
--   | tee /tmp/tx \
--   | jcli transaction to-message
--   )
-- cat /tmp/tx | jcli transaction info
-- rm /tmp/wit /tmp/tx
-- echo -e "$TX\n"
--
-- #  One input, one output, no change
-- echo $ROOTPRV0 | jcli transaction make-witness $FAKEIN0 --genesis-block-hash $BLOCK0 --type utxo > /tmp/wit
-- TX=$(jcli transaction new \
--   | jcli transaction add-input $FAKEIN0 1 14442 \
--   | jcli transaction add-output $ADDR0 1337 \
--   | jcli transaction finalize \
--   | jcli transaction add-witness /tmp/wit \
--   | jcli transaction seal \
--   | tee /tmp/tx \
--   | jcli transaction to-message
--   )
-- cat /tmp/tx | jcli transaction info
-- rm /tmp/wit /tmp/tx
-- echo -e "$TX\n"
--
-- #  3 inputs, 2 outputs, no change
-- echo $ROOTPRV0 | jcli transaction make-witness $FAKEIN0 --genesis-block-hash $BLOCK0 --type utxo > /tmp/wit0
-- echo $ROOTPRV1 | jcli transaction make-witness $FAKEIN1 --genesis-block-hash $BLOCK0 --type utxo > /tmp/wit1
-- TX=$(jcli transaction new \
--   | jcli transaction add-input $FAKEIN0 1 14442 \
--   | jcli transaction add-input $FAKEIN1 1 42 \
--   | jcli transaction add-input $FAKEIN0 2 123456789 \
--   | jcli transaction add-output $ADDR0 14 \
--   | jcli transaction add-output $ADDR1 123456 \
--   | jcli transaction finalize \
--   | jcli transaction add-witness /tmp/wit0 \
--   | jcli transaction add-witness /tmp/wit1 \
--   | jcli transaction add-witness /tmp/wit0 \
--   | jcli transaction seal \
--   | tee /tmp/tx \
--   | jcli transaction to-message
--   )
-- cat /tmp/tx | jcli transaction info
-- rm /tmp/wit* /tmp/tx
-- echo -e "$TX\n"
