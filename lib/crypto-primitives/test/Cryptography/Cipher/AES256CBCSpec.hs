{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Cryptography.Cipher.AES256CBCSpec
    ( spec
    ) where

import Prelude

import Cryptography.Cipher.AES256CBC
    ( CipherError (..)
    , CipherMode (..)
    , decrypt
    , encrypt
    , padPKCS7
    , unpadPKCS7
    )
import Cryptography.Hash.Core
    ( SHA256 (..)
    )
import Cryptography.KDF.PBKDF2
    ( PBKDF2Config (..)
    , generateKey
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString
    ( ByteString
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
    , chooseInt
    , oneof
    , property
    , suchThat
    , vectorOf
    , (===)
    )

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "Padding/unpadding roundtrip" $
        it "unpad . pad $ payload == payload" $ property $ \payload -> do
            let toPayload Nothing = Payload BS.empty
                toPayload (Just bs) = Payload bs
            toPayload ( padPKCS7 (unPayload payload) >>= unpadPKCS7 )
                === payload
    describe "Padding produces always payload that is multiple of 16 bytes" $
        it "(pad payload) % 16 == 0" $ property $ \payload -> do
            let toPayloadLen Nothing = 0
                toPayloadLen (Just bs) = BS.length bs
            (toPayloadLen ( (padPKCS7 $ unPayload payload)) ) `mod` 16
                === 0
    describe "encrypt/decrypt roundtrip with padding" $
        it "decrypt . encrypt $ payload == payload" $ property $
        \(CipherPaddingSetup payload' key' iv' salt') -> do
            let toPayload (Left EmptyPayload) = Right (BS.empty, salt')
                toPayload res = res
            toPayload (encrypt WithPadding key' iv' salt' payload' >>=
                decrypt WithPadding key' iv') === Right (payload', salt')

    describe "encrypt/decrypt roundtrip without padding" $
        it "decrypt . encrypt $ payload == payload" $ property $
        \(CipherRawSetup payload' key' iv' salt') -> do
            let toPayload (Left EmptyPayload) = Right (BS.empty, salt')
                toPayload res = res
            toPayload (encrypt WithoutPadding key' iv' salt' payload' >>=
                decrypt WithoutPadding key' iv') === Right (payload', salt')

    describe "encrypt with incorrect block size" $
        it "encrypt payload == error" $ property $
        \(CipherWrongSetup payload' key' iv' _salt') -> do
            encrypt WithoutPadding key' iv' Nothing payload' ===
                Left WrongPayloadSize

    describe "decrypt with incorrect block size" $
        it "decrypt payload == error" $ property $
        \(CipherWrongSetup payload' key' iv' _salt') -> do
            decrypt WithoutPadding key' iv' payload' ===
                Left WrongPayloadSize

    describe "Golden Tests obtained by openssl" $ do
        it "golden for key and iv with salt" $
            tohex (saltKey, saltIv) `shouldBe`
            ( "26A1A6F599173BAF863F97F2A18AF2FD8E99104E726D9EB682FCB7ED53517CF9"
            , "131BAB9240C168CC60A9F6DFA8CA5A6D"
            )

        it "golden for key and iv with salt" $
            tohex (noSaltKey, noSaltIv) `shouldBe`
            ( "E11244295150E6713CD76E9A5112347093BDB6ACBF0C8021ABAE29881130B210"
            , "6B7F0C406297F0D90E3BD65AD1FB94BA"
            )

         -- echo -n 0 | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "password" -nosalt -v
         -- ZMGELuBkqtHVw5pUA353vQ==
         -- bytes read   :        1
         -- bytes written:       25
        it "golden 1-byte payload" $
            checkEncryption GoldenCase
            { input = "0"
            , key = noSaltKey
            , iv = noSaltIv
            , salt = Nothing
            } `shouldBe`
            Right "ZMGELuBkqtHVw5pUA353vQ=="

        -- echo -n 00 | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "password" -nosalt -v
        -- KBO1WMy3NpqMmm+fEL2kbg==
        -- bytes read   :        2
        -- bytes written:       25
        it "golden 2-byte payload" $
            checkEncryption GoldenCase
            { input = "00"
            , key = noSaltKey
            , iv = noSaltIv
            , salt = Nothing
            } `shouldBe`
            Right "KBO1WMy3NpqMmm+fEL2kbg=="

        -- echo -n 00000 | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "password" -nosalt -v
        -- 49TERtYsEVKVgpJkUzqmJw==
        -- bytes read   :        5
        -- bytes written:       25
        it "golden 5-byte payload" $
            checkEncryption GoldenCase
            { input = "00000"
            , key = noSaltKey
            , iv = noSaltIv
            , salt = Nothing
            } `shouldBe`
            Right "49TERtYsEVKVgpJkUzqmJw=="

        -- echo -n 0000000000 | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "password" -nosalt -v
        -- XaYfTkqDajiW6hRc+SlJ8A==
        -- bytes read   :       10
        -- bytes written:       25
        it "golden 10-byte payload" $
            checkEncryption GoldenCase
            { input = "0000000000"
            , key = noSaltKey
            , iv = noSaltIv
            , salt = Nothing
            } `shouldBe`
            Right "XaYfTkqDajiW6hRc+SlJ8A=="

        -- echo -n 000000000000000 | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "password" -nosalt -v
        -- CXH8t8dSqroR4r4IiNAsfA==
        -- bytes read   :       15
        -- bytes written:       25
        it "golden 15-byte payload" $
            checkEncryption GoldenCase
            { input = "000000000000000"
            , key = noSaltKey
            , iv = noSaltIv
            , salt = Nothing
            } `shouldBe`
            Right "CXH8t8dSqroR4r4IiNAsfA=="

        -- echo -n 0000000000000000 | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "password" -nosalt -v
        -- hLArzI8DbqOXiEi6HqGQh8iZSRCztrdYpMEE1aj8ES8=
        -- bytes read   :       16
        -- bytes written:       45
        it "golden 16-byte payload" $
            checkEncryption GoldenCase
            { input = "0000000000000000"
            , key = noSaltKey
            , iv = noSaltIv
            , salt = Nothing
            } `shouldBe`
            Right "hLArzI8DbqOXiEi6HqGQh8iZSRCztrdYpMEE1aj8ES8="

        -- echo -n 00000000000000000000 | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "password" -nosalt -v
        -- hLArzI8DbqOXiEi6HqGQh96Z6cWqsqTJlbxfcWg/Nvo=
        -- bytes read   :       20
        -- bytes written:       45
        it "golden 20-byte payload" $
            checkEncryption GoldenCase
            { input = "00000000000000000000"
            , key = noSaltKey
            , iv = noSaltIv
            , salt = Nothing
            } `shouldBe`
            Right "hLArzI8DbqOXiEi6HqGQh96Z6cWqsqTJlbxfcWg/Nvo="

        -- echo -n 0000000000000000000000000000000 | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "password" -nosalt -v
        -- hLArzI8DbqOXiEi6HqGQh9VxdtWlCXnKS1n/vvCOwL0=
        -- bytes read   :       31
        -- bytes written:       45
        it "golden 31-byte payload" $
            checkEncryption GoldenCase
            { input = "0000000000000000000000000000000"
            , key = noSaltKey
            , iv = noSaltIv
            , salt = Nothing
            } `shouldBe`
            Right "hLArzI8DbqOXiEi6HqGQh9VxdtWlCXnKS1n/vvCOwL0="

        -- echo -n 00000000000000000000000000000000 | openssl enc -e -aes-256-cbc -pbkdf2 -iter 10000 -a -k "password" -nosalt -v
        -- hLArzI8DbqOXiEi6HqGQh/VxwHicJoMlVJqa8H2yg5xUYf9zCre088MTCb8jjIN1
        -- bytes read   :       32
        -- bytes written:       65
        it "golden 32-byte payload" $
            checkEncryption GoldenCase
            { input = "00000000000000000000000000000000"
            , key = noSaltKey
            , iv = noSaltIv
            , salt = Nothing
            } `shouldBe`
            Right
            "hLArzI8DbqOXiEi6HqGQh/VxwHicJoMlVJqa8H2yg5xUYf9zCre088MTCb8jjIN1"
  where
    --  We are using the following key and iv for goldens
    -- $ openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -S 3030303030303030 -k "password" -iter 10000 -P
    -- salt=3030303030303030
    -- key=26A1A6F599173BAF863F97F2A18AF2FD8E99104E726D9EB682FCB7ED53517CF9
    -- iv =131BAB9240C168CC60A9F6DFA8CA5A6D
    (saltKey, saltIv) =
        generateKey
            (PBKDF2Config SHA256 10000 32 16)
            "password"
            (fromHex "3030303030303030")

    -- $ openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -nosalt -k "password" -iter 10000 -P
    -- key=E11244295150E6713CD76E9A5112347093BDB6ACBF0C8021ABAE29881130B210
    -- iv =6B7F0C406297F0D90E3BD65AD1FB94BA
    (noSaltKey, noSaltIv) =
        generateKey
            (PBKDF2Config SHA256 10000 32 16)
            "password"
            Nothing

    fromHex = rightToMaybe . convertFromBase Base16 . T.encodeUtf8
    tohex :: (ByteString, ByteString) -> (Text, Text)
    tohex (a, b) =
        let convert = T.toUpper . T.decodeUtf8 . convertToBase Base16
        in (convert a, convert b)

    checkEncryption GoldenCase {..} =
        T.decodeUtf8 . convertToBase Base64
        <$> encrypt WithPadding key iv salt input

data GoldenCase = GoldenCase
    { input :: ByteString
    , key :: ByteString
    , iv :: ByteString
    , salt :: Maybe ByteString
    } deriving (Show, Eq)

newtype Payload = Payload
    { unPayload :: ByteString } deriving (Eq, Show)

data CipherPaddingSetup = CipherPaddingSetup
    { payloadPad :: ByteString
    , keyPad :: ByteString
    , ivPad :: ByteString
    , saltPad :: Maybe ByteString
    } deriving (Eq, Show)

data CipherRawSetup = CipherRawSetup
    { payloadRaw :: ByteString
    , keyRaw :: ByteString
    , ivRaw :: ByteString
    , saltRaw :: Maybe ByteString
    } deriving (Eq, Show)

data CipherWrongSetup = CipherWrongSetup
    { payloadWrong :: ByteString
    , keyWrong :: ByteString
    , ivWrong :: ByteString
    , saltWrong :: Maybe ByteString
    } deriving (Eq, Show)

instance Arbitrary Payload where
    arbitrary = do
        payloadLength <- chooseInt (1, 512)
        oneof
            [ Payload . BS.pack <$> vectorOf payloadLength arbitrary
            , pure $ Payload BS.empty
            ]

instance Arbitrary CipherPaddingSetup where
    arbitrary = do
        Payload payload' <- arbitrary
        key' <- BS.pack <$> vectorOf 32 arbitrary
        iv' <- BS.pack <$> vectorOf 16 arbitrary
        salt' <- oneof
            [ Just . BS.pack <$> vectorOf 8 arbitrary
            , pure Nothing
            ]
        pure $ CipherPaddingSetup payload' key' iv' salt'

instance Arbitrary CipherRawSetup where
    arbitrary = do
        lenMult <- chooseInt (1,256)
        payload' <- BS.pack <$> vectorOf (lenMult * 16) arbitrary
        key' <- BS.pack <$> vectorOf 32 arbitrary
        iv' <- BS.pack <$> vectorOf 16 arbitrary
        salt' <- oneof
            [ Just . BS.pack <$> vectorOf 8 arbitrary
            , pure Nothing
            ]
        pure $ CipherRawSetup payload' key' iv' salt'

instance Arbitrary CipherWrongSetup where
    arbitrary = do
        len <- chooseInt (1,512) `suchThat` (\p -> p `mod` 16 /= 0)
        payload' <- BS.pack <$> vectorOf len arbitrary
        key' <- BS.pack <$> vectorOf 32 arbitrary
        iv' <- BS.pack <$> vectorOf 16 arbitrary
        saltLen <- chooseInt (1,32) `suchThat` (\p -> p /= 8)
        salt' <- oneof
            [ Just . BS.pack <$> vectorOf saltLen arbitrary
            , pure Nothing
            ]
        pure $ CipherWrongSetup payload' key' iv' salt'
