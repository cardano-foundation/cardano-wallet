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
    , chooseInt
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
    BS.length (PKCS7.pad payload) `mod` 16 === 0

prop_pad_unpad :: Payload -> Property
prop_pad_unpad (Payload payload) =
    PKCS7.unpad (PKCS7.pad payload) === Just payload

newtype Payload = Payload
    { unPayload :: ByteString } deriving (Eq, Show)

instance Arbitrary Payload where
    arbitrary = do
        payloadLength <- chooseInt (1, 512)
        oneof
            [ Payload . BS.pack <$> vectorOf payloadLength arbitrary
            , pure $ Payload BS.empty
            ]
