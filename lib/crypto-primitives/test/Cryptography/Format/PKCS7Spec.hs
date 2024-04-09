module Cryptography.Format.PKCS7Spec
    ( spec
    ) where

import Prelude

import Data.ByteString
    ( ByteString
    )
import Data.Function
    ( (&) )
import Data.Maybe
    ( isJust, isNothing )
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
    , oneof
    , property
    , vectorOf
    , (===)
    )

import qualified Cryptography.Format.PKCS7 as PKCS7
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    it "prop_pad_length" $
        prop_pad_length
            & property
    it "prop_pad_unpad" $
        prop_pad_unpad
            & property

prop_pad_length :: Payload -> Property
prop_pad_length (Payload payload) =
    case maybePaddedPayload of
        Nothing ->
            payload === BS.empty
        Just paddedPayload ->
            BS.length paddedPayload `mod` 16 === 0
    & cover 10
        (isJust maybePaddedPayload)
        "isJust maybePaddedPayload"
    & cover 10
        (isNothing maybePaddedPayload)
        "isNothing maybePaddedPayload"
    & checkCoverage
  where
    maybePaddedPayload = PKCS7.pad payload

prop_pad_unpad :: Payload -> Property
prop_pad_unpad (Payload payload) =
    case maybePaddedPayload of
        Nothing ->
            payload === BS.empty
        Just paddedPayload ->
            PKCS7.unpad paddedPayload === Just payload
    & cover 10
        (isJust maybePaddedPayload)
        "isJust maybePaddedPayload"
    & cover 10
        (isNothing maybePaddedPayload)
        "isNothing maybePaddedPayload"
    & checkCoverage
  where
    maybePaddedPayload = PKCS7.pad payload

newtype Payload = Payload
    { unPayload :: ByteString } deriving (Eq, Show)

instance Arbitrary Payload where
    arbitrary = do
        payloadLength <- chooseInt (1, 512)
        oneof
            [ Payload . BS.pack <$> vectorOf payloadLength arbitrary
            , pure $ Payload BS.empty
            ]
