{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.API.Shelley.Network
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiEra (..), ApiNetworkParameters (..) )
import Cardano.Wallet.Pools
    ( EpochInfo (..) )
import Cardano.Wallet.Primitive.Types
    ( ExecutionUnitPrices (..) )
import Data.List
    ( (\\) )
import Data.Quantity
    ( Quantity (..), mkPercentage )
import Data.Ratio
    ( (%) )
import Test.Hspec
    ( Expectation, SpecWith, describe, shouldBe, shouldNotBe )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , RequestException
    , counterexample
    , epochLengthValue
    , expectField
    , expectResponseCode
    , maximumCollateralInputCountByEra
    , minimumCollateralPercentageByEra
    , request
    , securityParameterValue
    , slotLengthValue
    , verify
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec :: SpecWith Context
spec = describe "SHELLEY_NETWORK" $ do
    it "NETWORK_PARAMS - Able to fetch network parameters" $ \ctx -> do
        r <- request @ApiNetworkParameters ctx Link.getNetworkParams Default Empty
        expectResponseCode @IO HTTP.status200 r
        let Right d = fmap Quantity . mkPercentage $
                if _mainEra ctx >= ApiBabbage
                then 1 % 1
                else 3 % 4

        -- for Shelley desiredPoolNumber is node's nOpt protocol parameter
        -- in integration test setup it is 3
        let nOpt = 3
        let
            expectEraField
                :: (Maybe EpochInfo -> Expectation)
                -> ApiEra
                -> (HTTP.Status, Either RequestException ApiNetworkParameters)
                -> IO ()
            expectEraField toBe era = counterexample ("For era: " <> show era)
                . case era of
                    ApiByron -> expectField (#eras . #byron) toBe
                    ApiShelley -> expectField (#eras . #shelley) toBe
                    ApiAllegra -> expectField (#eras . #allegra) toBe
                    ApiMary -> expectField (#eras . #mary) toBe
                    ApiAlonzo -> expectField (#eras . #alonzo) toBe
                    ApiBabbage -> expectField (#eras . #babbage) toBe
                    ApiConway -> const $ pure () -- TODO https://input-output.atlassian.net/browse/ADP-2841

        let knownEras = [minBound .. _mainEra ctx]
        let unknownEras = [minBound .. maxBound] \\ knownEras
        -- exec prices values from alonzo-genesis.yml
        let execUnitPrices = Just ExecutionUnitPrices
                { pricePerStep = 721 % 10000000
                , pricePerMemoryUnit = 577 % 10000
                }
        let checkExecutionUnitPricesPresence
                :: ApiEra
                -> ((HTTP.Status, Either RequestException ApiNetworkParameters) -> IO ())
            checkExecutionUnitPricesPresence era =
                if era >= ApiAlonzo
                then expectField #executionUnitPrices (`shouldBe` execUnitPrices)
                else expectField #executionUnitPrices (`shouldBe` Nothing)

        verify r $
            [ expectField #decentralizationLevel
                (`shouldBe` d)
            , expectField #desiredPoolNumber
                (`shouldBe` nOpt)
            , expectField #slotLength
                (`shouldBe` Quantity slotLengthValue)
            , expectField #epochLength
                (`shouldBe` Quantity epochLengthValue)
            , expectField #securityParameter
                (`shouldBe` Quantity securityParameterValue)
            , expectField #activeSlotCoefficient
                (`shouldBe` Quantity 50.0)
            , expectField #maximumCollateralInputCount
                (`shouldBe` maximumCollateralInputCountByEra (_mainEra ctx))
            , expectField #minimumCollateralPercentage
                (`shouldBe` minimumCollateralPercentageByEra (_mainEra ctx))
            , expectField #maximumTokenBundleSize
                (`shouldBe` Quantity 5000)
            , checkExecutionUnitPricesPresence
                (_mainEra ctx)
            ]
            ++ map (expectEraField (`shouldNotBe` Nothing)) knownEras
            ++ map (expectEraField (`shouldBe` Nothing)) unknownEras
