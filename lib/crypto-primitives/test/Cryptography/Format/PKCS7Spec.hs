module Cryptography.Format.PKCS7Spec
    ( spec
    ) where

import Prelude

import Data.ByteString
    ( ByteString
    )
import Data.Function
    ( (&)
    )
import Test.Hspec
    ( Spec
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , checkCoverage
    , chooseInt
    , cover
    , property
    , vectorOf
    , (===)
    )

import qualified Cryptography.Format.PKCS7 as PKCS7
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    it "prop_pad_isPrefixOf" $
        prop_pad_isPrefixOf
            & property
    it "prop_pad_length" $
        prop_pad_length
            & property
    it "prop_pad_unpad" $
        prop_pad_unpad
            & property

prop_pad_isPrefixOf :: Payload -> Property
prop_pad_isPrefixOf (Payload payload) =
    payload `BS.isPrefixOf` PKCS7.pad payload
    ===
    True

prop_pad_length :: Payload -> Property
prop_pad_length (Payload payload) =
    BS.length (PKCS7.pad payload) `mod` 16 === 0
    & cover 5
        (BS.length payload `div` 16 == 0)
        "BS.length payload `div` 16 == 0"
    & cover 10
        (BS.length payload `div` 16 /= 0)
        "BS.length payload `div` 16 /= 0"
    & cover 5
        (BS.length payload `mod` 16 == 0)
        "BS.length payload `mod` 16 == 0"
    & cover 10
        (BS.length payload `mod` 16 /= 0)
        "BS.length payload `mod` 16 /= 0"
    & checkCoverage

prop_pad_unpad :: Payload -> Property
prop_pad_unpad (Payload payload) =
    PKCS7.unpad (PKCS7.pad payload) === Just payload

newtype Payload = Payload
    { unPayload :: ByteString } deriving (Eq, Show)

instance Arbitrary Payload where
    arbitrary = do
        payloadLength <- chooseInt (0, 256)
        Payload . BS.pack <$> vectorOf payloadLength arbitrary
