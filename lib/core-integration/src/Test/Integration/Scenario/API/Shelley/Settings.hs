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
    , updateMetadataSource
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiStakePool
    , ApiT (..)
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.Types
    ( Coin (..), PoolMetadataSource (..), Settings )
import Control.Monad.Catch
    ( MonadCatch )
import Control.Monad.IO.Class
    ( MonadIO )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Maybe
    ( isJust, isNothing )
import Data.Text
    ( Text )
import Data.Text.Class
    ( fromText )
import Test.Hspec
    ( SpecWith, describe, shouldBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , eventually
    , eventuallyUsingDelay
    , expectField
    , expectResponseCode
    , json
    , request
    , unsafeRequest
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
        updateMetadataSource ctx uri
        eventually "The settings are applied" $ do
            r2 <- request @(ApiT Settings) ctx Link.getSettings Default Empty
            verify r2
                [ expectResponseCode HTTP.status200
                , expectField (#getApiT . #poolMetadataSource)
                    (`shouldBe` (either (const (error "no")) id $ fromText
                        @PoolMetadataSource uri))
                ]

    it "SETTINGS_02 - Changing pool_metadata_source re-syncs metadata" $ \ctx -> do
        let toNone = "none"
            toDirect = "direct"
            getMetadata = fmap (view #metadata) . snd <$> unsafeRequest
                @[ApiStakePool] ctx (Link.listStakePools arbitraryStake) Empty
            delay = 500 * 1000
            timeout = 120

        updateMetadataSource ctx toNone
        verifyMetadataSource ctx FetchNone
        eventuallyUsingDelay delay timeout "1. There is no metadata" $
            getMetadata >>= (`shouldSatisfy` all isNothing)

        updateMetadataSource ctx toDirect
        verifyMetadataSource ctx FetchDirect
        eventuallyUsingDelay delay timeout "2. There is metadata" $
            getMetadata >>= (`shouldSatisfy` all isJust)

        updateMetadataSource ctx toNone
        verifyMetadataSource ctx FetchNone
        eventuallyUsingDelay delay timeout "3. There is no metadata" $
            getMetadata >>= (`shouldSatisfy` all isNothing)


updateMetadataSource :: (MonadIO m, MonadCatch m) => Context t -> Text -> m ()
updateMetadataSource ctx t = do
    r <- request @(ApiT Settings) ctx Link.putSettings Default payload
    expectResponseCode HTTP.status204 r
 where
   payload = Json [json| {
       "settings": {
           "pool_metadata_source": #{t}
            }
       } |]

verifyMetadataSource
    :: (MonadIO m, MonadCatch m)
    => Context t
    -> PoolMetadataSource
    -> m ()
verifyMetadataSource ctx s = do
    r <- request @(ApiT Settings) ctx Link.getSettings Default Empty
    expectResponseCode HTTP.status200 r
    expectField (#getApiT . #poolMetadataSource) (`shouldBe` s) r

arbitraryStake :: Maybe Coin
arbitraryStake = Just $ ada 10_000
  where ada = Coin . (1000*1000*)
