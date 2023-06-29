{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.HashSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text.Class
    ( TextDecodingError (..)
    , fromText
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , elements
    , vector
    , (===)
    )
import Test.Text.Roundtrip
    ( textRoundtrip
    )

import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Can perform roundtrip textual encoding & decoding" $ do
        textRoundtrip $ Proxy @(Hash "Account")
        textRoundtrip $ Proxy @(Hash "Block")
        textRoundtrip $ Proxy @(Hash "BlockHeader")
        textRoundtrip $ Proxy @(Hash "RewardAccount")
        textRoundtrip $ Proxy @(Hash "Genesis")
        textRoundtrip $ Proxy @(Hash "Tx")

    describe "Negative cases for types decoding" $ do
        it "fail fromText (@Hash \"Tx\")" $ do
            let err =
                    "Invalid tx hash: \
                    \expecting a hex-encoded value that is 32 bytes in length."
            fromText @(Hash "Tx") "----"
                === Left (TextDecodingError err)
        it "fail fromText (@Hash \"Genesis\")" $ do
            let err =
                    "Invalid genesis hash: \
                    \expecting a hex-encoded value that is 32 bytes in length."
            fromText @(Hash "Genesis") "----"
                === Left (TextDecodingError err)
        it "fail fromText (@Hash \"Block\")" $ do
            let err =
                    "Invalid block hash: \
                    \expecting a hex-encoded value that is 32 bytes in length."
            fromText @(Hash "Block") "----"
                === Left (TextDecodingError err)
        it "fail fromText (@Hash \"BlockHeader\")" $ do
            let err =
                    "Invalid blockHeader hash: \
                    \expecting a hex-encoded value that is 32 bytes in length."
            fromText @(Hash "BlockHeader") "----"
                === Left (TextDecodingError err)

instance Arbitrary (Hash "Genesis") where
    arbitrary = Hash . BS.pack <$> vector 32

instance Arbitrary (Hash "Block") where
    arbitrary = Hash . BS.pack <$> vector 32

instance Arbitrary (Hash "Account") where
    arbitrary = Hash . BS.pack <$> vector 32

instance Arbitrary (Hash "RewardAccount") where
    arbitrary = Hash . BS.pack <$> vector 28

instance Arbitrary (Hash "BlockHeader") where
    arbitrary = Hash . BS.pack <$> vector 32

instance Arbitrary (Hash "Tx") where
    -- No Shrinking
    arbitrary =
        elements
            [ Hash
                $ unsafeFromHex
                    "0000000000000000000000000000000000000000000000000000000000000001"
            , Hash
                $ unsafeFromHex
                    "0000000000000000000000000000000000000000000000000000000000000002"
            , Hash
                $ unsafeFromHex
                    "0000000000000000000000000000000000000000000000000000000000000003"
            ]
