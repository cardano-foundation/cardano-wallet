{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

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
    , PBKDF2Config (..)
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
            toKeyIV TestCase
            { algo = SHA256
            , iters = 10000
            , keyL = 32
            , ivL = 16
            , passwd = "password"
            , salt = Nothing
            } `shouldBe` OpenSSLOutput
            { key = "E11244295150E6713CD76E9A5112347093BDB6ACBF0C8021ABAE29881130B210"
            , iv = "6B7F0C406297F0D90E3BD65AD1FB94BA"
            }

        -- echo -n "password" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -nosalt -iter 5000 -P
        it "golden 2" $
            toKeyIV TestCase
            { algo = SHA256
            , iters = 5000
            , keyL = 32
            , ivL = 16
            , passwd = "password"
            , salt = Nothing
            } `shouldBe` OpenSSLOutput
            { key = "8C0A769589C080CD70BE80EE380E28C6DFB4430B19FBAAE0D718BFB4CD5228B1"
            , iv = "820EEC0C89FA3D9F4C52BB8137E7F937"
            }

        -- echo -n "password" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -md sha512 -nosalt -iter 10000 -P
        it "golden 3" $
            toKeyIV TestCase
            { algo = SHA512
            , iters = 10000
            , keyL = 32
            , ivL = 16
            , passwd = "password"
            , salt = Nothing
            } `shouldBe` OpenSSLOutput
            { key = "F4CA507C07D0BD31BC779A08756826A6FD9DD97D43AC25E4B29A0933ABEA03F3"
            , iv = "1FA1234792FF981F335BA91B0AB40E32"
            }

        -- echo -n "password" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -md sha512 -nosalt -iter 5000 -P
        it "golden 4" $
            toKeyIV TestCase
            { algo = SHA512
            , iters = 5000
            , keyL = 32
            , ivL = 16
            , passwd = "password"
            , salt = Nothing
            } `shouldBe` OpenSSLOutput
            { key = "ECC1A2E6F16D6420F9A953AD8A80FDD0D8AC12839E964C0D132C25047B430FFD"
            , iv = "51B2F57DB80A12BCE523577BACF75123"
            }

        -- echo -n "password" | openssl enc -aes-256-ecb -pbkdf2 -pass stdin -P -nosalt -iter 10000 -P
        it "golden 5" $
            toKeyIV TestCase
            { algo = SHA256
            , iters = 10000
            , keyL = 32
            , ivL = 0
            , passwd = "password"
            , salt = Nothing
            } `shouldBe` OpenSSLOutput
            { key = "E11244295150E6713CD76E9A5112347093BDB6ACBF0C8021ABAE29881130B210"
            , iv = ""
            }

        -- echo -n "password" | openssl enc -aes-256-ecb -pbkdf2 -pass stdin -P -nosalt -iter 5000 -P
        it "golden 6" $
            toKeyIV TestCase
            { algo = SHA256
            , iters = 5000
            , keyL = 32
            , ivL = 0
            , passwd = "password"
            , salt = Nothing
            } `shouldBe` OpenSSLOutput
            { key = "8C0A769589C080CD70BE80EE380E28C6DFB4430B19FBAAE0D718BFB4CD5228B1"
            , iv = ""
            }

        -- echo -n "password" | openssl enc -aes-256-ecb -pbkdf2 -pass stdin -P -md sha512 -nosalt -iter 10000 -P
        it "golden 7" $
            toKeyIV TestCase
            { algo = SHA512
            , iters = 10000
            , keyL = 32
            , ivL = 0
            , passwd = "password"
            , salt = Nothing
            } `shouldBe` OpenSSLOutput
            { key = "F4CA507C07D0BD31BC779A08756826A6FD9DD97D43AC25E4B29A0933ABEA03F3"
            , iv = ""
            }

        -- echo -n "password" | openssl enc -aes-256-ecb -pbkdf2 -pass stdin -P -md sha512 -nosalt -iter 5000 -P
        it "golden 8" $
            toKeyIV TestCase
            { algo = SHA512
            , iters = 5000
            , keyL = 32
            , ivL = 0
            , passwd = "password"
            , salt = Nothing
            } `shouldBe` OpenSSLOutput
            { key = "ECC1A2E6F16D6420F9A953AD8A80FDD0D8AC12839E964C0D132C25047B430FFD"
            , iv = ""
            }

        -- Note: "00000000" is "3030303030303030" in hex
        -- echo -n "password" | openssl enc -aes-256-ecb -pbkdf2 -pass stdin -P -S 3030303030303030 -iter 10000 -P
        it "golden 9" $
            toKeyIV TestCase
            { algo = SHA256
            , iters = 10000
            , keyL = 32
            , ivL = 0
            , passwd = "password"
            , salt = Just "00000000"
            } `shouldBe` OpenSSLOutput
            { key = "26A1A6F599173BAF863F97F2A18AF2FD8E99104E726D9EB682FCB7ED53517CF9"
            , iv = ""
            }

        -- echo -n "password" | openssl enc -aes-256-ecb -pbkdf2 -pass stdin -P -S 3030303030303030 -iter 5000 -P
        it "golden 10" $
            toKeyIV TestCase
            { algo = SHA256
            , iters = 5000
            , keyL = 32
            , ivL = 0
            , passwd = "password"
            , salt = Just "00000000"
            } `shouldBe` OpenSSLOutput
            { key = "3BE342BC36004A2F34B8FAD4BDCA91AE3495A946CF7C0DD789EDE283AF8A3EE1"
            , iv = ""
            }

        -- echo -n "password" | openssl enc -aes-256-ecb -pbkdf2 -pass stdin -P -md sha512 -S 3030303030303030 -iter 10000 -P
        it "golden 11" $
            toKeyIV TestCase
            { algo = SHA512
            , iters = 10000
            , keyL = 32
            , ivL = 0
            , passwd = "password"
            , salt = Just "00000000"
            } `shouldBe` OpenSSLOutput
            { key = "A0BEF9E8434D518A2D9BCCF7883808BEDC5093E678AC7AC3CA1C6F7071B49575"
            , iv = ""
            }

        -- echo -n "password" | openssl enc -aes-256-ecb -pbkdf2 -pass stdin -P -md sha512 -S 3030303030303030 -iter 5000 -P
        it "golden 12" $
            toKeyIV TestCase
            { algo = SHA512
            , iters = 5000
            , keyL = 32
            , ivL = 0
            , passwd = "password"
            , salt = Just "00000000"
            } `shouldBe` OpenSSLOutput
            { key = "0DB80B6BC6D151C4FA474E8533DD2D4172208B895BBDB1D7ED564B1817F50B51"
            , iv = ""
            }

        -- echo -n "password" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -S 3030303030303030 -iter 10000 -P
        it "golden 13" $
            toKeyIV TestCase
            { algo = SHA256
            , iters = 10000
            , keyL = 32
            , ivL = 16
            , passwd = "password"
            , salt = Just "00000000"
            } `shouldBe` OpenSSLOutput
            { key = "26A1A6F599173BAF863F97F2A18AF2FD8E99104E726D9EB682FCB7ED53517CF9"
            , iv = "131BAB9240C168CC60A9F6DFA8CA5A6D"
            }

        -- echo -n "password" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -S 3030303030303030 -iter 5000 -P
        it "golden 14" $
            toKeyIV TestCase
            { algo = SHA256
            , iters = 5000
            , keyL = 32
            , ivL = 16
            , passwd = "password"
            , salt = Just "00000000"
            } `shouldBe` OpenSSLOutput
            { key = "3BE342BC36004A2F34B8FAD4BDCA91AE3495A946CF7C0DD789EDE283AF8A3EE1"
            , iv = "345CFDD8532C26C590107A30008200F6"
            }

        -- echo -n "password" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -md sha512 -S 3030303030303030 -iter 10000 -P
        it "golden 15" $
            toKeyIV TestCase
            { algo = SHA512
            , iters = 10000
            , keyL = 32
            , ivL = 16
            , passwd = "password"
            , salt = Just "00000000"
            } `shouldBe` OpenSSLOutput
            { key = "A0BEF9E8434D518A2D9BCCF7883808BEDC5093E678AC7AC3CA1C6F7071B49575"
            , iv = "8CEAB15C11F31E8BD3907D24C2D60E94"
            }

        -- echo -n "password" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -P -md sha512 -S 3030303030303030 -iter 5000 -P
        it "golden 16" $
            toKeyIV TestCase
            { algo = SHA512
            , iters = 5000
            , keyL = 32
            , ivL = 16
            , passwd = "password"
            , salt = Just "00000000"
            } `shouldBe` OpenSSLOutput
            { key = "0DB80B6BC6D151C4FA474E8533DD2D4172208B895BBDB1D7ED564B1817F50B51"
            , iv = "BD53567BFDA090E3A7C9BB54E34E4A8F"
            }

        -- echo -n "password" | openssl enc -aes-256-ecb -pbkdf2 -pass stdin -saltlen 15 -S 616464726573732D68617368696E67 -iter 500 -md sha512 -P
        -- Note: "address-hashing" is "616464726573732D68617368696E67" in hex
        -- Note: saltlen option needs to be specified as 8 bytes salt length is a default value and enforced. The option is available from OpenSSL 3.2 onwards.
        it "golden Byron" $
            toKeyIV TestCase
            { algo = SHA512
            , iters = 500
            , keyL = 32
            , ivL = 0
            , passwd = "password"
            , salt = Just "address-hashing"
            } `shouldBe` OpenSSLOutput
            { key = "6D3FF0C9AAE42F26B518C4E9F98D1A1EAB59B55E5A82F78F66DFC7CAA956DCA0"
            , iv = ""
            }

        -- echo -n "password" | openssl enc -aes-256-ecb -pbkdf2 -pass stdin -S 6D6E656D6F6E6963 -iter 2048 -md sha512 -P
        -- Note: "mnemonic" is "6D6E656D6F6E6963" in hex
        -- Note: Icarus uses 64 byte output key but here we check for 32 byte case.
        it "golden Icarus" $
            toKeyIV TestCase
            { algo = SHA512
            , iters = 2048
            , keyL = 32
            , ivL = 0
            , passwd = "password"
            , salt = Just "mnemonic"
            } `shouldBe` OpenSSLOutput
            { key = "E0FB520D01112056904730F762AD263B806953F1FE5B4662C97B1F301BB5F862"
            , iv = ""
            }

        -- echo -n "password" | openssl enc -aes-256-cbc -pbkdf2 -pass stdin -S 6162636465666768 -iter 10000 -md sha256 -P
        -- Note: "abcdefgh" is "6162636465666768" in hex. Salt is 8-byte.
        it "golden CIP83" $
            toKeyIV TestCase
            { algo = SHA256
            , iters = 10000
            , keyL = 32
            , ivL = 16
            , passwd = "password"
            , salt = Just "abcdefgh"
            } `shouldBe` OpenSSLOutput
            { key = "6BBCD65DA84D7BB1D214908270352A56FF25B375E53B7D3F237330666CCBF0DF"
            , iv = "FD40C29618D438E3658388C52EB7974E"
            }
  where
    toKeyIV TestCase {..} =
        tohex (generateKey (PBKDF2Config algo iters keyL ivL) passwd salt)
    tohex :: (ByteString, ByteString) -> OpenSSLOutput
    tohex (a, b) =
        let convert = T.toUpper . T.decodeUtf8 . convertToBase Base16
        in OpenSSLOutput (convert a) (convert b)

data TestCase h = TestCase
    { algo :: h
    , iters :: Int
    , keyL :: Int
    , ivL :: Int
    , passwd :: ByteString
    , salt :: Maybe ByteString
    } deriving (Show, Eq)

data OpenSSLOutput = OpenSSLOutput
    { key :: Text
    , iv :: Text
    } deriving (Show, Eq)
