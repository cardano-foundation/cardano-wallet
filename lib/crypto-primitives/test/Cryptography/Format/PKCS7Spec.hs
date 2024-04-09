module Cryptography.Format.PKCS7Spec
    ( spec
    ) where

import Prelude

import Cryptography.Format.PKCS7
    ( padPKCS7
    , unpadPKCS7
    )
import Data.ByteString
    ( ByteString
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , chooseInt
    , oneof
    , property
    , vectorOf
    , (===)
    )

import qualified Data.ByteString as BS

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

newtype Payload = Payload
    { unPayload :: ByteString } deriving (Eq, Show)

instance Arbitrary Payload where
    arbitrary = do
        payloadLength <- chooseInt (1, 512)
        oneof
            [ Payload . BS.pack <$> vectorOf payloadLength arbitrary
            , pure $ Payload BS.empty
            ]
