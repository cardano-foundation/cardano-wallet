{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.API.Shelley.Network
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiNetworkParameters (..) )
import Data.Quantity
    ( Quantity (..), mkPercentage )
import Data.Ratio
    ( (%) )
import Test.Hspec
    ( SpecWith, it, shouldBe )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , expectField
    , expectResponseCode
    , request
    , verify
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t. SpecWith (Context t)
spec = do
    it "NETWORK_PARAMS - Able to fetch network parameters" $ \ctx -> do
        r <- request @ApiNetworkParameters ctx Link.getNetworkParams Default Empty
        expectResponseCode @IO HTTP.status200 r
        let Right d = Quantity <$> mkPercentage (3 % 4)
        -- for Shelley desiredPoolNumber is node's nOpt protocol parameter
        -- in integration test setup it is 3
        let nOpt = 3
        let minUtxoValue = Quantity 0
        verify r
            [ expectField (#decentralizationLevel) (`shouldBe` d)
            , expectField (#desiredPoolNumber) (`shouldBe` nOpt)
            , expectField (#minimumUtxoValue) (`shouldBe` minUtxoValue)
            ]
