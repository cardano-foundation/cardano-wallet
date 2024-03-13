{-# LANGUAGE FlexibleContexts #-}

module Cryptography.KDF.PBKDF2Spec
    ( spec
    ) where

import Prelude

import Cryptography.Hash.Core
    ( SHA256 (..)
    , SHA512 (..)
    )
import Cryptography.KDF.PBKDF2
    ( generateKey
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertToBase
    )
import Data.ByteString
    ( ByteString
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

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    describe "Golden Tests obtained by openssl" $ do
        -- echo -n "password" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -nosalt -iter 10000 -P
        it "golden 1" $
            tohex (generateKey SHA256 10000 (32,16) "password" Nothing) `shouldBe`
            OpenSSLOutput
            { key = "E11244295150E6713CD76E9A5112347093BDB6ACBF0C8021ABAE29881130B210"
            , iv = "6B7F0C406297F0D90E3BD65AD1FB94BA"
            }

        -- echo -n "password" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -nosalt -iter 5000 -P
        it "golden 2" $
            tohex (generateKey SHA256 5000 (32,16) "password" Nothing) `shouldBe`
            OpenSSLOutput
            { key = "8C0A769589C080CD70BE80EE380E28C6DFB4430B19FBAAE0D718BFB4CD5228B1"
            , iv = "820EEC0C89FA3D9F4C52BB8137E7F937"
            }

        -- echo -n "password" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -md sha512 -nosalt -iter 10000 -P
        it "golden 3" $
            tohex (generateKey SHA512 10000 (32,16) "password" Nothing) `shouldBe`
            OpenSSLOutput
            { key = "F4CA507C07D0BD31BC779A08756826A6FD9DD97D43AC25E4B29A0933ABEA03F3"
            , iv = "1FA1234792FF981F335BA91B0AB40E32"
            }

        -- echo -n "password" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -md sha512 -nosalt -iter 5000 -P
        it "golden 4" $
            tohex (generateKey SHA512 5000 (32,16) "password" Nothing) `shouldBe`
            OpenSSLOutput
            { key = "ECC1A2E6F16D6420F9A953AD8A80FDD0D8AC12839E964C0D132C25047B430FFD"
            , iv = "51B2F57DB80A12BCE523577BACF75123"
            }
  where
    tohex :: (ByteString, ByteString) -> OpenSSLOutput
    tohex (a, b) =
        let convert = T.toUpper . T.decodeUtf8 . convertToBase Base16
        in OpenSSLOutput (convert a) (convert b)


data OpenSSLOutput = OpenSSLOutput
    { key :: Text
    , iv :: Text
    } deriving (Show, Eq)
