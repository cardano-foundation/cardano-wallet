{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Primitive.Types.PoolIdSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.PoolId
    ( PoolId (..)
    , decodePoolIdBech32
    , encodePoolIdBech32
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( forAll
    , vector
    , withMaxSuccess
    , (===)
    )

import qualified Data.ByteString as BS

spec :: Spec
spec = describe "Cardano.Wallet.Primitive.Types.PoolId" $ do
    it "Can roundtrip {decode,encode}PoolIdBech32"
        $ withMaxSuccess 1000
        $ forAll (PoolId . BS.pack <$> vector 32)
        $ \pid ->
            decodePoolIdBech32 (encodePoolIdBech32 pid) === Right pid
