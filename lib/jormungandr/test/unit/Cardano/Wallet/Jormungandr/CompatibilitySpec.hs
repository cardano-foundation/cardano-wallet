{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( xpubFromBytes )
import Cardano.Wallet.Api.Types
    ( ApiAddress (..), ApiT (..), DecodeAddress (..), EncodeAddress (..) )
import Cardano.Wallet.Jormungandr.Compatibility
    ()
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , networkDiscriminantVal
    )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey (..), KnownNetwork (..), publicKeySize )
import Cardano.Wallet.Primitive.Types
    ( Address (..), AddressState (..), ProtocolMagic (..), ShowFmt (..) )
import Data.Aeson
    ( FromJSON (..) )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Either
    ( isRight )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..) )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Spec
    , SpecWith
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldSatisfy
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , frequency
    , oneof
    , property
    , vector
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Roundtrip
    ( httpApiDataRoundtrip )

import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Write as CBOR
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Test.Utils.Roundtrip as Utils

spec :: Spec
spec = do
    let jsonRoundtripAndGolden = Utils.jsonRoundtripAndGolden
            ($(getTestData) </> "Cardano" </> "Wallet" </> "Api")
    describe
        "can perform roundtrip JSON serialization & deserialization, \
        \and match existing golden files" $ do
            jsonRoundtripAndGolden $ Proxy @(ApiAddress ('Testnet 0))

    describe "HTTPApiData Roundtrip" $ do
        httpApiDataRoundtrip $ Proxy @(ApiT Address, Proxy 'Mainnet)
        httpApiDataRoundtrip $ Proxy @(ApiT Address, Proxy ('Testnet 0))

    describe "verify JSON parsing failures too" $ do
        it "ApiT Address" $ do
            let msg = "Error in $: Unable to decode Address: \
                    \encoding is neither Bech32 nor Base58."
            Aeson.parseEither parseJSON [aesonQQ|"-----"|]
                `shouldBe` (Left @String @(ApiT Address, Proxy ('Testnet 0)) msg)

    describe "encodeAddress & decodeAddress (Mainnet)" $ do
        let proxy = Proxy @'Mainnet
        it "decodeAddress . encodeAddress = pure" $
            withMaxSuccess 1000 $ property $ \(ApiT a, _ :: Proxy 'Mainnet) ->
                (ShowFmt <$> decodeAddress @'Mainnet (encodeAddress @'Mainnet a))
                    === Right (ShowFmt a)
        negativeTest proxy "ta1sdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677ztw225s"
            ("This address belongs to another network. Network is: "
            <> show (networkDiscriminantVal @'Mainnet) <> ".")
        negativeTest proxy "EkxDbkPo"
            "Unable to decode Address: neither Bech32-encoded nor a valid Byron \
            \Address."
        negativeTest proxy ".%14'"
            ("Unable to decode Address: encoding is neither Bech32 nor Base58.")
        negativeTest proxy "ca1qv8qurswpc8qurswpc8qurs7xnyen"
            "Invalid address length (14): expected either 33 or 65 bytes."
        negativeTest proxy
            "ca1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqscdket"
            ("This type of address is not supported: [107].")
        negativeTest proxy
            "ca1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqqgzqvz\
            \q2ps8pqys5zcvp58q7yq3zgf3g9gkzuvpjxsmrsw3u8eqwxpnc0"
            ("This type of address is not supported: [107].")
        -- NOTE:
        -- Data below have been generated with [jcli](https://github.com/input-output-hk/jormungandr/tree/master/doc/jcli)
        -- as described in the annex at the end of the file.
        goldenTestAddr proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            ]
            "addr1qdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677z5t3m7d"
        goldenTestAddr proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            ]
            "addr1q00e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr7n5fgsv"
        goldenTestAddr proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            , "b24e70b0c2ceeb24cc9f28f386478c73aa71c05a95a0119bb91dd8e89c3592ae"
            ]
            "addr1q3aa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677\
            \rvjwwzcv9nhtynxf728nserccua2w8q949dqzxdmj8wcazwrty4wvuat2l"
        goldenTestAddr proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            , "402abff6065c847115ad22ff6b0d3a85fd69a6fcc32ed76aa8cadb305b0c51a7"
            ]
            "addr1qn0e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr\
            \7sp2hlmqvhyywy266ghldvxn4p0adxn0esew6a423jkmxpdsc5d8n8hz07"

    describe "encodeAddress & decodeAddress (Testnet)" $ do
        let proxy = Proxy @('Testnet 0)
        it "decodeAddress . encodeAddress = pure" $
            withMaxSuccess 1000 $ property $ \(ApiT a, _ :: Proxy ('Testnet 0)) ->
                (ShowFmt <$> decodeAddress @('Testnet 0) (encodeAddress @('Testnet 0) a))
                    === Right (ShowFmt a)
        negativeTest proxy "ca1qdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677zqx4le2"
            ("This address belongs to another network. Network is: "
            <> show (networkDiscriminantVal @('Testnet 0)) <> ".")
        negativeTest proxy "EkxDbkPo"
            "Unable to decode Address: neither Bech32-encoded nor a valid Byron \
            \Address."
        negativeTest proxy ".%14'"
            ("Unable to decode Address: encoding is neither Bech32 nor Base58.")
        negativeTest proxy "ta1sv8qurswpc8qurswpc8qurs2l0ech"
            "Invalid address length (14): expected either 33 or 65 bytes."
        negativeTest proxy
            "ta1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jq8ygppa"
            ("This type of address is not supported: [107].")
        negativeTest proxy
            "ta1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqqgzqvz\
            \q2ps8pqys5zcvp58q7yq3zgf3g9gkzuvpjxsmrsw3u8eq9lcgc2"
            ("This type of address is not supported: [107].")
        goldenTestAddr proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            ]
            "addr1sdaa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677zgltetp"
        goldenTestAddr proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            ]
            "addr1s00e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr70qn29q"
        goldenTestAddr proxy
            [ "7bd5386c31ac31ba7076856500cf26f85d4695b80f183c7a53e3f28419d6bde1"
            , "b24e70b0c2ceeb24cc9f28f386478c73aa71c05a95a0119bb91dd8e89c3592ae"
            ]
            "addr1s3aa2wrvxxkrrwnsw6zk2qx0ymu96354hq83s0r6203l9pqe6677\
            \rvjwwzcv9nhtynxf728nserccua2w8q949dqzxdmj8wcazwrty4wkdnx06"
        goldenTestAddr proxy
            [ "df9f08672a3a94778229b91daa981538883e1535d666dc10e63b438f44c63e3f"
            , "402abff6065c847115ad22ff6b0d3a85fd69a6fcc32ed76aa8cadb305b0c51a7"
            ]
            "addr1sn0e7zr89gafgauz9xu3m25cz5ugs0s4xhtxdhqsuca58r6ycclr\
            \7sp2hlmqvhyywy266ghldvxn4p0adxn0esew6a423jkmxpdsc5d8fke02m"

    describe "Golden test addresses" $ do
        it "Byron / Random (Mainnet)" $
            decodeAddress @'Mainnet
                "DdzFFzCqrhstkaXBhux3ALL9wqvP3Nkz8QE5qKwFbqkmTL6zyKpc\
                \FpqXJLkgVhRTYkf5F7mh3q6bAv5hWYjhSV1gekjEJE8XFeZSganv"
            `shouldSatisfy` isRight

        it "Byron / Icarus (Mainnet)" $
            decodeAddress @'Mainnet
                "Ae2tdPwUPEZ9mpZBa3pN4CJH3xRA4cPr1HTUE1dVBF5JKNvkAKUxdK8f5L9"
              `shouldSatisfy` isRight

        it "Byron / Random (Testnet)" $
            decodeAddress @('Testnet 1097911063)
                "37btjrVyb4KFg2FzVkfmBWgue1qqC7oHmFNVTWYgLTHEE9wGC6xizioGw\
                \sYPAtPbDTrvtnV7vUXAsmP7pTMx7X95AcwfUAqoLJpGJ4eaosUHGBjta4"
              `shouldSatisfy` isRight

        it "Byron / Icarus (Testnet)" $
            decodeAddress @('Testnet 1097911063)
                "2cWKMJemoBajA7ji3xQvAnrSESRyceRVnj\
                \5j9kj1Tb9pzGoY5jPc142iPXfaix1DbbDF7"
              `shouldSatisfy` isRight

-- | Generate addresses from the given keys and compare the result with an
-- expected output obtained from jcli (see appendix below)
goldenTestAddr
    :: forall n. (DelegationAddress n JormungandrKey, EncodeAddress n)
    => Proxy n
    -> [ByteString]
    -> Text
    -> SpecWith ()
goldenTestAddr _proxy pubkeys expected = it ("golden test: " <> T.unpack expected) $ do
    case traverse (convertFromBase Base16) pubkeys of
        Right [spendingKey] -> do
            let xpub = JormungandrKey (CC.XPub spendingKey chainCode)
            let addr = encodeAddress @n (paymentAddress @n xpub)
            addr `shouldBe` expected
        Right [spendingKey, delegationKey] -> do
            let xpubSpending = JormungandrKey (CC.XPub spendingKey chainCode)
            let xpubDeleg = JormungandrKey (CC.XPub delegationKey chainCode)
            let addr = encodeAddress @n (delegationAddress @n xpubSpending xpubDeleg)
            addr `shouldBe` expected
        _ ->
            expectationFailure "goldenTestAddr: provided invalid inputs public keys"
  where
    chainCode = CC.ChainCode "<ChainCode is not used by singleAddressFromKey>"

negativeTest
    :: forall n. DecodeAddress n
    => Proxy n
    -> Text
    -> String
    -> SpecWith ()
negativeTest _proxy bytes msg = it ("decodeAddress failure: " <> msg) $
    decodeAddress @n bytes === Left (TextDecodingError msg)

instance Arbitrary a => Arbitrary (ApiT a) where
    arbitrary = ApiT <$> arbitrary

instance Arbitrary (ApiAddress t) where
    shrink _ = []
    arbitrary = ApiAddress
        <$> fmap (, Proxy @t) arbitrary
        <*> arbitrary

instance Arbitrary AddressState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Address where
    arbitrary = (getApiT . fst)
        <$> arbitrary @(ApiT Address, Proxy ('Testnet 0))

instance {-# OVERLAPS #-} Arbitrary (ApiT Address, Proxy 'Mainnet) where
    arbitrary = do
        addr <- ApiT <$> frequency
            [ (10, genAddress @'Mainnet)
            , (1, genLegacyAddress Nothing)
            ]
        return (addr, Proxy)

instance {-# OVERLAPS #-} Arbitrary (ApiT Address, Proxy ('Testnet 0)) where
    arbitrary = do
        addr <- ApiT <$> frequency
            [ (10, genAddress @('Testnet 0))
            , (1, genLegacyAddress (Just (ProtocolMagic 0)))
            ]
        return (addr, Proxy)

genAddress
    :: forall (network :: NetworkDiscriminant). (KnownNetwork network)
    => Gen Address
genAddress = oneof
    [ (\bytes -> Address (BS.pack (addrSingle @network:bytes)))
        <$> vector publicKeySize
    , (\bytes -> Address (BS.pack (addrGrouped @network:bytes)))
        <$> vector (2*publicKeySize)
    , (\bytes -> Address (BS.pack (addrAccount @network:bytes)))
        <$> vector publicKeySize
    ]

genLegacyAddress
    :: Maybe ProtocolMagic
    -> Gen Address
genLegacyAddress pm = do
    bytes <- BS.pack <$> vector 64
    let (Just key) = xpubFromBytes bytes
    pure $ Address
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress key
        $ maybe [] (pure . CBOR.encodeProtocolMagicAttr) pm
