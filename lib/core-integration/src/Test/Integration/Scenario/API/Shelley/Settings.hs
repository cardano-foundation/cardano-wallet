{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Shelley.Settings
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiT (..), DecodeAddress, DecodeStakeAddress, EncodeAddress (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.Types
    ( PoolMetadataSource, Settings )
import Data.Text.Class
    ( fromText )
import Test.Hspec
    ( SpecWith, describe, shouldBe )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , eventually
    , expectField
    , expectResponseCode
    , json
    , request
    , verify
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n t.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n ShelleyKey
    , PaymentAddress n IcarusKey
    , PaymentAddress n ByronKey
    ) => SpecWith (Context t)
spec = describe "SHELLEY_SETTINGS" $ do
    it "SETTINGS_01 - Can put and read settings" $ \ctx -> do
        let uri = "http://smash.it"
            payload = Json [json| {
                "settings": {
                    "pool_metadata_source": #{uri}
                     }
                } |]
        r <- request @(ApiT Settings) ctx Link.putSettings Default
            payload
        expectResponseCode @IO HTTP.status204 r
        eventually "The settings are applied" $ do
            r2 <- request @(ApiT Settings) ctx Link.getSettings Default Empty
            verify r2
                [ expectResponseCode @IO HTTP.status200
                , expectField (#getApiT . #poolMetadataSource)
                    (`shouldBe` (either (const (error "no")) id $ fromText
                        @PoolMetadataSource uri))
                ]
