{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jormungandr.Scenario.API.StakePools
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiStakePool )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress, EncodeAddress )
import Control.Monad
    ( forM_ )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , blocks
    , eventually
    , eventuallyUsingDelay
    , expectErrorMessage
    , expectListItemFieldBetween
    , expectListItemFieldEqual
    , expectListSizeEqual
    , expectResponseCode
    , listStakePoolsEp
    , metrics
    , request
    , stake
    , verify
    )
import Test.Integration.Framework.TestData
    ( errMsg405 )

import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t. (EncodeAddress t, DecodeAddress t) => SpecWith (Context t)
spec = do
    it "STAKE_POOLS_LIST_01 - List stake pools" $ \ctx -> do
        eventually $ do
            r <- request @[ApiStakePool] ctx listStakePoolsEp Default Empty
            expectResponseCode HTTP.status200 r
            -- With the current genesis.yaml we have 1 pool with 1 lovelace, and
            -- an epoch length of 3, meaning it will have produced either 1 or
            -- 2 blocks in the current epoch.
            verify r
                [ expectListSizeEqual 1
                , expectListItemFieldEqual 0
                    (metrics . stake) 1
                , expectListItemFieldBetween 0
                    (metrics . blocks) (1, 2)
                ]

    it "STAKE_POOLS_LIST_02 - May fail on epoch boundaries" $ \ctx -> do
    -- We should be able to catch the stake-pool data in an un-synced state
    -- when we enter into a new epoch. The results should then be
    -- unavailible.
    --
    -- This might take a few tries (epoch changes), so it is only feasible
    -- to test with very short epochs.
        let ms = 1000
        eventuallyUsingDelay (50*ms) $ do
            r <- request @[ApiStakePool] ctx listStakePoolsEp Default Empty
            verify r
                [ expectResponseCode HTTP.status503
                , expectErrorMessage
                    "I can't list stake pools yet because I need to scan the \
                    \blockchain for metrics first. I'm at"
                ]

    describe "STAKE_POOLS_LIST_03 - v2/stake-pools - Methods Not Allowed" $ do
        let methods = ["POST", "PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS"]
        forM_ methods $ \method -> it (show method) $ \ctx -> do
            r <- request @ApiStakePool ctx (method, "v2/stake-pools") Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r
