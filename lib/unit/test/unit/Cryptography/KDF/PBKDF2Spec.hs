{-# LANGUAGE FlexibleContexts #-}

module Cryptography.KDF.PBKDF2Spec
    ( spec
    ) where

import Prelude

import Cryptography.Hash.Core
    ( SHA256 (..)
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
            ( "E11244295150E6713CD76E9A5112347093BDB6ACBF0C8021ABAE29881130B210"
            , "6B7F0C406297F0D90E3BD65AD1FB94BA"
            )
  where
    tohex :: (ByteString, ByteString) -> (Text, Text)
    tohex (a, b) =
        let convert = T.toUpper . T.decodeUtf8 . convertToBase Base16
        in (convert a, convert b)
