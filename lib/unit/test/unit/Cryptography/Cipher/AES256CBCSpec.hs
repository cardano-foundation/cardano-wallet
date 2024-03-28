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
    , paddingPKCS7
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
    describe "Padding produces always payload that is multiple of 16 bytes" $
        it "(pad payload) % 16 == 0" $ property $ \payload -> do
            let toPayloadLen Nothing = 0
                toPayloadLen (Just bs) = BS.length bs
            (toPayloadLen ( (paddingPKCS7 $ unPayload payload)) ) `mod` 16 === 0
    describe "encrypt/decrypt roundtrip with padding" $
        it "decrypt . encrypt $ payload == payload" $ property $
        \(CipherPaddingSetup payload' key' iv') -> do
            let toPayload (Left WrongPayload) = Right BS.empty
                toPayload res = res
            toPayload (encrypt WithPadding key' iv' payload' >>=
                decrypt WithPadding key' iv') === Right payload'

newtype Payload = Payload
    { unPayload :: ByteString } deriving (Eq, Show)

data CipherPaddingSetup = CipherPaddingSetup
    { payload :: ByteString
    , key :: ByteString
    , iv :: ByteString
    } deriving (Eq, Show)

instance Arbitrary Payload where
    arbitrary = do
        payloadLength <- chooseInt (1, 250)
        oneof [ Payload . BS.pack <$> vectorOf payloadLength arbitrary
              , pure $ Payload BS.empty
              ]

instance Arbitrary CipherPaddingSetup where
    arbitrary = do
        Payload payload' <- arbitrary
        key' <- BS.pack <$> vectorOf 32 arbitrary
        iv' <- BS.pack <$> vectorOf 16 arbitrary
        pure $ CipherPaddingSetup payload' key' iv'
