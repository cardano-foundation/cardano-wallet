{-# LANGUAGE FlexibleContexts #-}

module Cardano.Wallet.Primitive.Types.MetadataEncryptionSpec
    ( spec
    ) where

import Cardano.Wallet.Primitive.Types.MetadataEncryption
    ( ErrMetadataDecryption (..)
    , ErrMetadataEncryption (..)
    , cip20MetadataKey
    , cip83EncryptMethodKey
    , cip83EncryptPayloadKey
    , cip83EncryptPayloadValue
    , fromMetadataEncrypted
    , toMetadataEncrypted
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Char
    ( isAlphaNum
    )
import Data.Either
    ( isLeft
    )
import Data.Either.Combinators
    ( mapLeft
    , rightToMaybe
    )
import Data.Function
    ( (&)
    )
import Data.Text
    ( Text
    )
import Test.Hspec
    ( Expectation
    , Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , UnicodeString (..)
    , chooseInt
    , property
    , suchThat
    , vectorOf
    , (===)
    )
import Prelude

import qualified Cardano.Api as Cardano
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "metadata encrypt/decrypt roundtrips" $ do
        it "fromMetadataEncrypted . toMetadataEncrypted $ payload == payload"
            $ prop_roundtrip
            & property

        it "fromMetadataEncrypted fails for different passphrase"
            $ prop_passphrase
            & property

        it
            "the valid result of toMetadataEncrypted exhibits the expected characteristics"
            $ prop_structure_after_enc
            & property

    describe "toMetadataEncrypted openssl goldens" $ do
        -- echo -n '["secret data"]' | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "cardano" -nosalt
        -- Fm/+xoZBA24yp8Vz548NAg==
        it "short msg - no salt" $ do
            let schemaBefore =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaList
                                            [Cardano.TxMetaText "secret data"]
                                        )
                                    ]
                                )
                            ]
                schemaAfter =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaList
                                            [Cardano.TxMetaText "Fm/+xoZBA24yp8Vz548NAg=="]
                                        )
                                    ,
                                        ( Cardano.TxMetaText "enc"
                                        , Cardano.TxMetaText "basic"
                                        )
                                    ]
                                )
                            ]
            toMetadataEncrypted "cardano" schemaBefore Nothing
                `shouldBe` Right schemaAfter
            fromMetadataEncrypted "cardano" schemaAfter
                `shouldBe` Left ErrMissingSalt

        it "short msg - no salt wrong value structure" $ do
            let schemaBefore =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaText "secret data"
                                        )
                                    ]
                                )
                            ]
            toMetadataEncrypted "cardano" schemaBefore Nothing
                `shouldBe` Left ErrIncorrectRawMetadata

        -- \$ echo -n '["secret data that is long enough to produce more than 64 bytes"]' | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "cardano" -nosalt
        -- +8ruwpQolMU4wznBR5LYQEyke/SlJ7mkU+1LEXs2vSC8gegvjWESqnWK1Tw59cFt
        -- CKO3g/d6fGA2jOU7JDYlC1qf+mdDKlGHbPKCV41Fofs=
        it "long msg - no salt" $ do
            let schemaBefore =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaList
                                            [ Cardano.TxMetaText
                                                "secret data that is long enough to produce more \
                                                \than 64 bytes"
                                            ]
                                        )
                                    ]
                                )
                            ]
                schemaAfter =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaList
                                            [ Cardano.TxMetaText
                                                "+8ruwpQolMU4wznBR5LYQEyke/SlJ7mkU+1LEXs2vSC8gegvjWESqnWK1Tw59cFt"
                                            , Cardano.TxMetaText "CKO3g/d6fGA2jOU7JDYlC1qf+mdDKlGHbPKCV41Fofs="
                                            ]
                                        )
                                    ,
                                        ( Cardano.TxMetaText "enc"
                                        , Cardano.TxMetaText "basic"
                                        )
                                    ]
                                )
                            ]
            toMetadataEncrypted "cardano" schemaBefore Nothing
                `shouldBe` Right schemaAfter
            fromMetadataEncrypted "cardano" schemaAfter
                `shouldBe` Left ErrMissingSalt

        -- \$ echo -n '["Invoice-No: 123456789","Order-No: 7654321","Email: john@doe.com"]' | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "cardano" -nosalt
        -- IBcjjGQ7akr/CV2Zb0HtCvEPQNndZujCZ7iaFGMjOX3q3PJg5aRUvHgO3gPnDzYE
        -- 7jFsGUK1bCdwsrn8kqI92NccbG8oAtPJUktZTTcO/bg=
        it "cip msg - no salt" $ do
            let schemaBefore =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaList
                                            [ Cardano.TxMetaText "Invoice-No: 123456789"
                                            , Cardano.TxMetaText "Order-No: 7654321"
                                            , Cardano.TxMetaText "Email: john@doe.com"
                                            ]
                                        )
                                    ]
                                )
                            ]
                schemaAfter =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaList
                                            [ Cardano.TxMetaText
                                                "IBcjjGQ7akr/CV2Zb0HtCvEPQNndZujCZ7iaFGMjOX3q3PJg5aRUvHgO3gPnDzYE"
                                            , Cardano.TxMetaText "7jFsGUK1bCdwsrn8kqI92NccbG8oAtPJUktZTTcO/bg="
                                            ]
                                        )
                                    ,
                                        ( Cardano.TxMetaText "enc"
                                        , Cardano.TxMetaText "basic"
                                        )
                                    ]
                                )
                            ]
            toMetadataEncrypted "cardano" schemaBefore Nothing
                `shouldBe` Right schemaAfter
            fromMetadataEncrypted "cardano" schemaAfter
                `shouldBe` Left ErrMissingSalt

        -- \$ echo -n '["secret data"]' | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "cardano" -S 3030303030303030
        -- U2FsdGVkX18wMDAwMDAwMKg9+BnuLSqx880pgF+owzo=
        it "short msg - salted" $ do
            let schemaBefore =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaList
                                            [Cardano.TxMetaText "secret data"]
                                        )
                                    ]
                                )
                            ]
                schemaAfter =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaList
                                            [Cardano.TxMetaText "U2FsdGVkX18wMDAwMDAwMKg9+BnuLSqx880pgF+owzo="]
                                        )
                                    ,
                                        ( Cardano.TxMetaText "enc"
                                        , Cardano.TxMetaText "basic"
                                        )
                                    ]
                                )
                            ]
                saltM = fromHexToM "3030303030303030"
            toMetadataEncrypted "cardano" schemaBefore saltM
                `shouldBe` Right schemaAfter
            fromMetadataEncrypted "cardano" schemaAfter
                `shouldBe` Right schemaBefore

        -- \$ echo -n '["secret data that is long enough to produce more than 64 bytes"]' | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "cardano" -S 3030303030303030
        -- U2FsdGVkX18wMDAwMDAwMK3WTtGcfCw96FEEQJct+JQfvpq824MACKzRPNqul83i
        -- Jxd3aOenCM/IBadPmEcDVPyg+f/tszUp0KO8uzRxKTnY1bO4rqEKEQfu1GkAz7wF
        it "long msg - salted" $ do
            let schemaBefore =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaList
                                            [ Cardano.TxMetaText
                                                "secret data that is long enough to produce more than 64 bytes"
                                            ]
                                        )
                                    ]
                                )
                            ]
                schemaAfter =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaList
                                            [ Cardano.TxMetaText
                                                "U2FsdGVkX18wMDAwMDAwMK3WTtGcfCw96FEEQJct+JQfvpq824MACKzRPNqul83i"
                                            , Cardano.TxMetaText
                                                "Jxd3aOenCM/IBadPmEcDVPyg+f/tszUp0KO8uzRxKTnY1bO4rqEKEQfu1GkAz7wF"
                                            ]
                                        )
                                    ,
                                        ( Cardano.TxMetaText "enc"
                                        , Cardano.TxMetaText "basic"
                                        )
                                    ]
                                )
                            ]
                saltM = fromHexToM "3030303030303030"
            toMetadataEncrypted "cardano" schemaBefore saltM
                `shouldBe` Right schemaAfter
            fromMetadataEncrypted "cardano" schemaAfter
                `shouldBe` Right schemaBefore

        -- \$ $ echo -n '["Invoice-No: 123456789","Order-No: 7654321","Email: john@doe.com"]' | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "cardano" -S 3030303030303030
        -- U2FsdGVkX18wMDAwMDAwMFlOS4b0tXrZA7U5aQaHeI/sP74h84EPEjGv0wl4D8Do
        -- +SIXXn04a9xkoFHk4ZH281nIfH5lpClsO16p2vRpSsdBDFO78aTPX3bsHsRE0L2A
        it "cip msg - salted" $ do
            let schemaBefore =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaList
                                            [ Cardano.TxMetaText "Invoice-No: 123456789"
                                            , Cardano.TxMetaText "Order-No: 7654321"
                                            , Cardano.TxMetaText "Email: john@doe.com"
                                            ]
                                        )
                                    ]
                                )
                            ]
                schemaAfter =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaList
                                            [ Cardano.TxMetaText
                                                "U2FsdGVkX18wMDAwMDAwMFlOS4b0tXrZA7U5aQaHeI/sP74h84EPEjGv0wl4D8Do"
                                            , Cardano.TxMetaText
                                                "+SIXXn04a9xkoFHk4ZH281nIfH5lpClsO16p2vRpSsdBDFO78aTPX3bsHsRE0L2A"
                                            ]
                                        )
                                    ,
                                        ( Cardano.TxMetaText "enc"
                                        , Cardano.TxMetaText "basic"
                                        )
                                    ]
                                )
                            ]
                saltM = fromHexToM "3030303030303030"
            toMetadataEncrypted "cardano" schemaBefore saltM
                `shouldBe` Right schemaAfter
            fromMetadataEncrypted "cardano" schemaAfter
                `shouldBe` Right schemaBefore

        it "msg wrong label - no salt" $ do
            let schemaBefore =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 675
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaList
                                            [ Cardano.TxMetaText "Invoice-No: 123456789"
                                            , Cardano.TxMetaText "Order-No: 7654321"
                                            , Cardano.TxMetaText "Email: john@doe.com"
                                            ]
                                        )
                                    ]
                                )
                            ]
            toMetadataEncrypted "cardano" schemaBefore Nothing
                `shouldBe` Left ErrIncorrectRawMetadata

        it "msg without 'msg field' - no salt" $ do
            let schemaBefore =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msgs"
                                        , Cardano.TxMetaList
                                            [ Cardano.TxMetaText "Invoice-No: 123456789"
                                            , Cardano.TxMetaText "Order-No: 7654321"
                                            , Cardano.TxMetaText "Email: john@doe.com"
                                            ]
                                        )
                                    ]
                                )
                            ]
            toMetadataEncrypted "cardano" schemaBefore Nothing
                `shouldBe` Left ErrIncorrectRawMetadata

    describe "fromMetadataEncrypted incorrect payload" $ do
        it "expecting only TxMetaText in TxMetaList of 'msg'" $ do
            let schemaAfter =
                    Cardano.TxMetadata
                        $ Map.fromList
                            [
                                ( 674
                                , Cardano.TxMetaMap
                                    [
                                        ( Cardano.TxMetaText "field"
                                        , Cardano.TxMetaNumber 123
                                        )
                                    ,
                                        ( Cardano.TxMetaText "msg"
                                        , Cardano.TxMetaList
                                            [ Cardano.TxMetaText "Fm/+xoZBA24yp8Vz548NAg=="
                                            , Cardano.TxMetaNumber 123
                                            ]
                                        )
                                    ,
                                        ( Cardano.TxMetaText "enc"
                                        , Cardano.TxMetaText "basic"
                                        )
                                    ]
                                )
                            ]
            fromMetadataEncrypted "cardano" schemaAfter
                `shouldBe` Left ErrMissingValidEncryptionPayload

fromHexToM :: Text -> Maybe ByteString
fromHexToM = rightToMaybe . convertFromBase Base16 . T.encodeUtf8

newtype MetadataPassword = MetadataPassword {unPassword :: ByteString}
    deriving (Eq, Show)

instance Arbitrary MetadataPassword where
    arbitrary = do
        pwdLen <- chooseInt (5, 10)
        chars <- vectorOf pwdLen arbitrary `suchThat` L.all isAlphaNum
        pure $ MetadataPassword $ B8.pack chars

data TestingSetup = TestingSetup
    { payload :: Cardano.TxMetadata
    , password :: MetadataPassword
    , passwordOther :: MetadataPassword
    , salt :: ByteString
    }
    deriving (Eq, Show)

newtype Msg = Msg {getMsg :: Text}
    deriving (Eq, Show)

instance Arbitrary Msg where
    arbitrary = do
        txt <-
            (T.pack . getUnicodeString <$> arbitrary) `suchThat` (not . T.null)
        pure $ Msg txt

instance Arbitrary TestingSetup where
    arbitrary = do
        msgNum <- chooseInt (1, 10)
        txts <- vectorOf msgNum (getMsg <$> arbitrary)
        pwd1 <- arbitrary
        pwd2 <- arbitrary `suchThat` (/= pwd1)
        salt' <- BS.pack <$> vectorOf 8 arbitrary
        let metadata toEncrypt =
                Cardano.TxMetadata
                    $ Map.fromList
                        [
                            ( 674
                            , Cardano.TxMetaMap
                                [
                                    ( Cardano.TxMetaText "field"
                                    , Cardano.TxMetaNumber 123
                                    )
                                ,
                                    ( Cardano.TxMetaText "msg"
                                    , Cardano.TxMetaList toEncrypt
                                    )
                                ]
                            )
                        ]
        pure
            $ TestingSetup
                { payload = metadata $ Cardano.TxMetaText <$> txts
                , password = pwd1
                , passwordOther = pwd2
                , salt = salt'
                }

prop_roundtrip :: TestingSetup -> Property
prop_roundtrip (TestingSetup payload' (MetadataPassword pwd') _ salt') = do
    ( mapLeft
            (const ErrMissingValidEncryptionPayload)
            (toMetadataEncrypted pwd' payload' (Just salt'))
            >>= fromMetadataEncrypted pwd'
        )
        === Right payload'

prop_passphrase :: TestingSetup -> Expectation
prop_passphrase
    ( TestingSetup
            payload'
            (MetadataPassword pwd1)
            (MetadataPassword pwd2)
            salt'
        ) = do
        ( mapLeft
                (const ErrMissingValidEncryptionPayload)
                (toMetadataEncrypted pwd1 payload' (Just salt'))
                >>= fromMetadataEncrypted pwd2
            )
            `shouldSatisfy` isLeft

prop_structure_after_enc :: TestingSetup -> Expectation
prop_structure_after_enc (TestingSetup payload' (MetadataPassword pwd') _ salt') = do
    let hasMsgWithList (Cardano.TxMetaText k, Cardano.TxMetaList _) =
            k == cip83EncryptPayloadKey
        hasMsgWithList _ = False
        hasEncPair (Cardano.TxMetaText k, Cardano.TxMetaText v) =
            k == cip83EncryptMethodKey && v == cip83EncryptPayloadValue
        hasEncPair _ = False
    let hasCharacteristics (Cardano.TxMetadata themap) =
            case Map.lookup cip20MetadataKey themap of
                Just (Cardano.TxMetaMap kvs) ->
                    any hasMsgWithList kvs && any hasEncPair kvs
                _ -> False

    (hasCharacteristics <$> toMetadataEncrypted pwd' payload' (Just salt'))
        `shouldBe` Right True
