{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.Primitive.Types.MetadataEncryptionSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.MetadataEncryption
    ( ErrMetadataDecryption (..)
    , ErrMetadataEncryption (..)
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
import Data.Either
    ( isRight
    , fromRight
    )
import Data.Either.Combinators
    ( rightToMaybe
    )
import Data.Text
    ( Text
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , UnicodeString (..)
    , chooseInt
    , property
    , suchThat
    , vectorOf
    , (===)
    , (==>)
    )

import qualified Cardano.Api as Cardano
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "metadata encrypt/decrypt roundtrip" $
        it "fromMetadataEncrypted . toMetadataEncrypted $ payload == payload" $ property $
        \(TestingSetup payload' pwd' _ salt') -> do
            isRight (toMetadataEncrypted pwd' payload' (Just salt')) ==>
                (fromMetadataEncrypted pwd'
                 (fromRight metadataNotValid (toMetadataEncrypted pwd' payload' (Just salt'))))
                === Right payload'

    describe "toMetadataEncrypted openssl goldens" $ do
        -- $ echo -n '"secret data"' | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "cardano" -nosalt
        -- vBSywXY+WGcrckHUCyjJcQ==
        it "short msg - no salt" $ do
            let schemaBefore =
                    Cardano.TxMetadata $
                    Map.fromList
                    [ ( 674
                      , Cardano.TxMetaMap
                        [ ( Cardano.TxMetaText "field"
                          , Cardano.TxMetaNumber 123
                          )
                        , ( Cardano.TxMetaText "msg"
                          , Cardano.TxMetaText "secret data"
                          )
                        ]
                      )
                    ]
                schemaAfter =
                    Cardano.TxMetadata $
                    Map.fromList
                    [ ( 674
                      , Cardano.TxMetaMap
                        [ ( Cardano.TxMetaText "field"
                          , Cardano.TxMetaNumber 123
                          )
                        , ( Cardano.TxMetaText "msg"
                          , Cardano.TxMetaList
                            [Cardano.TxMetaText "vBSywXY+WGcrckHUCyjJcQ=="]
                          )
                        , ( Cardano.TxMetaText "enc"
                          , Cardano.TxMetaText "basic"
                          )
                        ]
                      )
                    ]
            toMetadataEncrypted "cardano" schemaBefore Nothing
                `shouldBe` Right schemaAfter
            fromMetadataEncrypted "cardano" schemaAfter
                `shouldBe` Left ErrMissingSalt

        -- $ echo -n '"secret data that is long enough to produce more than 64 bytes"' | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "cardano" -nosalt
        -- OLSOdRF+P56rW9gUopHcs0HHcdmPP5ujhSuB+r84VJgvsMOsqmIZx2etosnkyOc8
        -- ygjbu25gCdhJh7iEpAJVaA==
        it "long msg - no salt" $ do
            let schemaBefore =
                    Cardano.TxMetadata $
                    Map.fromList
                    [ ( 674
                      , Cardano.TxMetaMap
                        [ ( Cardano.TxMetaText "field"
                          , Cardano.TxMetaNumber 123
                          )
                        , ( Cardano.TxMetaText "msg"
                          , Cardano.TxMetaText
                            "secret data that is long enough to produce more \
                            \than 64 bytes"
                          )
                        ]
                      )
                    ]
                schemaAfter =
                    Cardano.TxMetadata $
                    Map.fromList
                    [ ( 674
                      , Cardano.TxMetaMap
                        [ ( Cardano.TxMetaText "field"
                          , Cardano.TxMetaNumber 123
                          )
                        , ( Cardano.TxMetaText "msg"
                          , Cardano.TxMetaList
                            [ Cardano.TxMetaText "OLSOdRF+P56rW9gUopHcs0HHcdmPP5ujhSuB+r84VJgvsMOsqmIZx2etosnkyOc8"
                            , Cardano.TxMetaText "ygjbu25gCdhJh7iEpAJVaA=="
                            ]
                          )
                        , ( Cardano.TxMetaText "enc"
                          , Cardano.TxMetaText "basic"
                          )
                        ]
                      )
                    ]
            toMetadataEncrypted "cardano" schemaBefore Nothing
                `shouldBe` Right schemaAfter
            fromMetadataEncrypted "cardano" schemaAfter
                `shouldBe` Left ErrMissingSalt

        -- $ echo -n '["Invoice-No: 123456789","Order-No: 7654321","Email: john@doe.com"]' | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "cardano" -nosalt
        -- IBcjjGQ7akr/CV2Zb0HtCvEPQNndZujCZ7iaFGMjOX3q3PJg5aRUvHgO3gPnDzYE
        -- 7jFsGUK1bCdwsrn8kqI92NccbG8oAtPJUktZTTcO/bg=
        it "cip msg - no salt" $ do
            let schemaBefore =
                    Cardano.TxMetadata $
                    Map.fromList
                    [ ( 674
                      , Cardano.TxMetaMap
                        [ ( Cardano.TxMetaText "field"
                          , Cardano.TxMetaNumber 123
                          )
                        , ( Cardano.TxMetaText "msg"
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
                    Cardano.TxMetadata $
                    Map.fromList
                    [ ( 674
                      , Cardano.TxMetaMap
                        [ ( Cardano.TxMetaText "field"
                          , Cardano.TxMetaNumber 123
                          )
                        , ( Cardano.TxMetaText "msg"
                          , Cardano.TxMetaList
                            [ Cardano.TxMetaText "IBcjjGQ7akr/CV2Zb0HtCvEPQNndZujCZ7iaFGMjOX3q3PJg5aRUvHgO3gPnDzYE"
                            , Cardano.TxMetaText "7jFsGUK1bCdwsrn8kqI92NccbG8oAtPJUktZTTcO/bg="
                            ]
                          )
                        , ( Cardano.TxMetaText "enc"
                          , Cardano.TxMetaText "basic"
                          )
                        ]
                      )
                    ]
            toMetadataEncrypted "cardano" schemaBefore Nothing
                `shouldBe` Right schemaAfter
            fromMetadataEncrypted "cardano" schemaAfter
                `shouldBe` Left ErrMissingSalt

        -- $ $ echo -n '"secret data"' | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "cardano" -S 3030303030303030
        -- U2FsdGVkX18wMDAwMDAwMF0ea/2sHeptB3SvZtgc600=
        it "short msg - salted" $ do
            let schemaBefore =
                    Cardano.TxMetadata $ Map.fromList
                    [ ( 674
                      , Cardano.TxMetaMap
                        [ ( Cardano.TxMetaText "field"
                          , Cardano.TxMetaNumber 123
                          )
                        , ( Cardano.TxMetaText "msg"
                          , Cardano.TxMetaText "secret data"
                          )
                        ]
                        )
                    ]
                schemaAfter =
                    Cardano.TxMetadata $
                    Map.fromList
                    [ ( 674
                      , Cardano.TxMetaMap
                        [ ( Cardano.TxMetaText "field"
                          , Cardano.TxMetaNumber 123
                          )
                        , ( Cardano.TxMetaText "msg"
                          , Cardano.TxMetaList
                            [ Cardano.TxMetaText "U2FsdGVkX18wMDAwMDAwMF0ea/2sHeptB3SvZtgc600="
                            ]
                          )
                        , ( Cardano.TxMetaText "enc"
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

        -- $ echo -n '"secret data that is long enough to produce more than 64 bytes"' | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "cardano" -S 3030303030303030
        -- U2FsdGVkX18wMDAwMDAwMPNdhZQON/Hlwqvk4+sNRCa90QrAVpIGUlWgZhgNlwKh
        -- PbR/qyT2q0tejHQmsHdORif5rvZYTzJGsTutA0RIcFU=
        it "long msg - salted" $ do
            let schemaBefore =
                    Cardano.TxMetadata $ Map.fromList
                    [ ( 674
                      , Cardano.TxMetaMap
                        [ ( Cardano.TxMetaText "field"
                          , Cardano.TxMetaNumber 123
                          )
                        , ( Cardano.TxMetaText "msg"
                          , Cardano.TxMetaText "secret data that is long enough to produce more than 64 bytes"
                          )
                        ]
                      )
                    ]
                schemaAfter =
                    Cardano.TxMetadata $
                    Map.fromList
                    [ ( 674
                      , Cardano.TxMetaMap
                        [ ( Cardano.TxMetaText "field"
                          , Cardano.TxMetaNumber 123
                          )
                        , ( Cardano.TxMetaText "msg"
                          , Cardano.TxMetaList
                            [ Cardano.TxMetaText "U2FsdGVkX18wMDAwMDAwMPNdhZQON/Hlwqvk4+sNRCa90QrAVpIGUlWgZhgNlwKh"
                            , Cardano.TxMetaText "PbR/qyT2q0tejHQmsHdORif5rvZYTzJGsTutA0RIcFU="
                            ]
                          )
                        , ( Cardano.TxMetaText "enc"
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

        -- $ $ echo -n '["Invoice-No: 123456789","Order-No: 7654321","Email: john@doe.com"]' | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "cardano" -S 3030303030303030
        -- U2FsdGVkX18wMDAwMDAwMFlOS4b0tXrZA7U5aQaHeI/sP74h84EPEjGv0wl4D8Do
        -- +SIXXn04a9xkoFHk4ZH281nIfH5lpClsO16p2vRpSsdBDFO78aTPX3bsHsRE0L2A
        it "cip msg - salted" $ do
            let schemaBefore =
                    Cardano.TxMetadata $
                    Map.fromList
                    [ ( 674
                      , Cardano.TxMetaMap
                        [ ( Cardano.TxMetaText "field"
                          , Cardano.TxMetaNumber 123
                          )
                        , ( Cardano.TxMetaText "msg"
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
                    Cardano.TxMetadata $
                    Map.fromList
                    [ ( 674
                      , Cardano.TxMetaMap
                        [ ( Cardano.TxMetaText "field"
                          , Cardano.TxMetaNumber 123
                          )
                        , ( Cardano.TxMetaText "msg"
                          , Cardano.TxMetaList
                            [ Cardano.TxMetaText "U2FsdGVkX18wMDAwMDAwMFlOS4b0tXrZA7U5aQaHeI/sP74h84EPEjGv0wl4D8Do"
                            , Cardano.TxMetaText "+SIXXn04a9xkoFHk4ZH281nIfH5lpClsO16p2vRpSsdBDFO78aTPX3bsHsRE0L2A"
                            ]
                          )
                        , ( Cardano.TxMetaText "enc"
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
                    Cardano.TxMetadata $ Map.fromList
                    [ ( 675
                      , Cardano.TxMetaMap
                        [ ( Cardano.TxMetaText "field"
                          , Cardano.TxMetaNumber 123
                          )
                        , ( Cardano.TxMetaText "msg"
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
                    Cardano.TxMetadata $
                    Map.fromList
                    [ ( 674
                      , Cardano.TxMetaMap
                        [ ( Cardano.TxMetaText "field"
                          , Cardano.TxMetaNumber 123
                          )
                        , ( Cardano.TxMetaText "msgs"
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

fromHexToM :: Text -> Maybe ByteString
fromHexToM = rightToMaybe . convertFromBase Base16 . T.encodeUtf8

data TestingSetup = TestingSetup
    { payload :: Cardano.TxMetadata
    , password :: ByteString
    , passwordOther :: ByteString
    , salt :: ByteString
    } deriving (Eq, Show)

data Msg = Msg {getMsg :: Text}

instance Arbitrary Msg where
    arbitrary = do
        txt <- (T.pack . getUnicodeString <$> arbitrary) `suchThat` (not . T.null)
        pure $ Msg txt

instance Arbitrary TestingSetup where
    arbitrary = do
        msgNum <- chooseInt (1,10)
        txts <- vectorOf msgNum (getMsg <$> arbitrary)
        pwdLen1 <- chooseInt (5,10)
        pwdLen2 <- chooseInt (5,10)
        pwd1 <- BS.pack <$> vectorOf pwdLen1 arbitrary
        pwd2 <- (BS.pack <$> vectorOf pwdLen2 arbitrary) `suchThat` (/= pwd1)
        salt' <- BS.pack <$> vectorOf 8 arbitrary
        let metadata toEncrypt =
                Cardano.TxMetadata $ Map.fromList
                [ ( 674
                  , Cardano.TxMetaMap
                      [ ( Cardano.TxMetaText "field"
                        , Cardano.TxMetaNumber 123
                        )
                      , ( Cardano.TxMetaText "msg"
                        , Cardano.TxMetaList toEncrypt
                        )
                      ]
                  )
                ]
        pure $ TestingSetup
            { payload = metadata $ Cardano.TxMetaText <$> txts
            , password = pwd1
            , passwordOther = pwd2
            , salt = salt'
            }

metadataNotValid :: Cardano.TxMetadata
metadataNotValid =
    Cardano.TxMetadata $
    Map.fromList
    [ ( 674
      , Cardano.TxMetaMap
        [ ( Cardano.TxMetaText "field"
          , Cardano.TxMetaNumber 123
          )
        , ( Cardano.TxMetaText "msgs"
          , Cardano.TxMetaList
            [ Cardano.TxMetaText "Invoice-No: 123456789"
            , Cardano.TxMetaText "Order-No: 7654321"
            , Cardano.TxMetaText "Email: john@doe.com"
            ]
          )
        ]
      )
    ]
