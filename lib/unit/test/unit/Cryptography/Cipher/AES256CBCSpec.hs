{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Cryptography.Cipher.AES256CBCSpec
    ( spec
    ) where

import Prelude

import Cryptography.Cipher.AES256CBC
    ( paddingPKCS7
    , unpaddingPKCS7
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
            toPayload ( (paddingPKCS7 $ unPayload payload) >>= unpaddingPKCS7 ) === payload

newtype Payload = Payload
    { unPayload :: ByteString } deriving (Eq, Show)

instance Arbitrary Payload where
    arbitrary = do
        payloadLength <- chooseInt (1, 250)
        oneof [ Payload . BS.pack <$> vectorOf payloadLength arbitrary
              , pure $ Payload BS.empty
              ]
