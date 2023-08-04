{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Integration.Scenario.API.Byron.Network
  ( spec
  )
where

import Cardano.Wallet.Api.Link qualified as Link
import Cardano.Wallet.Api.Types
  ( ApiNetworkParameters (..)
  )
import Data.Quantity
  ( Quantity (..)
  , mkPercentage
  )
import Data.Ratio
  ( (%)
  )
import Network.HTTP.Types.Status qualified as HTTP
import Test.Hspec
  ( SpecWith
  , describe
  , it
  , shouldBe
  )
import Test.Integration.Framework.DSL
  ( Context (..)
  , Headers (..)
  , Payload (..)
  , expectField
  , expectResponseCode
  , request
  , verify
  )
import Prelude

spec :: SpecWith Context
spec = describe "BYRON_NETWORK" $ do
  it "NETWORK_PARAMS - Able to fetch network parameters" $ \ctx -> do
    r <- request @ApiNetworkParameters ctx Link.getNetworkParams Default Empty
    expectResponseCode @IO HTTP.status200 r
    let
      Right d = Quantity <$> mkPercentage (0 % 1)
    -- for Byron desiredPoolNumber is 0
    let
      nOpt = 0
    verify
      r
      [ expectField (#decentralizationLevel) (`shouldBe` d)
      , expectField (#desiredPoolNumber) (`shouldBe` nOpt)
      ]
