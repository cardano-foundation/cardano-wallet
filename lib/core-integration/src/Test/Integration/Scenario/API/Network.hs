{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.API.Network
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiBlockReference (..), ApiNetworkInformation, ApiT (..) )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (unSlotNo), SyncProgress (..) )
import Control.Monad
    ( forM_ )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , emptyWallet
    , eventually
    , expectErrorMessage
    , expectEventually'
    , expectFieldBetween
    , expectFieldEqual
    , expectResponseCode
    , getFromResponse
    , networkInfoEp
    , request
    , state
    , syncProgress
    , verify
    )
import Test.Integration.Framework.TestData
    ( errMsg405, errMsg406 )

import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t. SpecWith (Context t)
spec = do
    it "NETWORK - Can query network information" $ \ctx -> do
        eventually $ do
            r <- request @ApiNetworkInformation ctx networkInfoEp Default Empty
            let (ApiBlockReference _ sl _) = getFromResponse #nodeTip r
            verify r
                [ expectFieldEqual syncProgress Ready
                , expectFieldBetween (#nodeTip . #height)
                    ( Quantity 0
                    , Quantity $ fromIntegral $ unSlotNo $ (getApiT sl) + 1
                    )
                ]
    it "NETWORK2 - Wallet has the same tip as network/information" $ \ctx -> do
        let getNetworkInfo = request @ApiNetworkInformation ctx networkInfoEp Default Empty
        w <- emptyWallet ctx
        eventually $ do
            sync <- getNetworkInfo
            verify sync [ expectFieldEqual syncProgress Ready ]
        r <- getNetworkInfo
        let epochNum = getFromResponse (#nodeTip . #epochNumber . #getApiT) r
        let slotNum = getFromResponse (#nodeTip . #slotNumber . #getApiT) r
        let blockHeight = getFromResponse (#nodeTip . #height) r

        expectEventually' ctx state Ready w
        expectEventually' ctx (#tip . #epochNumber . #getApiT) epochNum w
        expectEventually' ctx (#tip . #slotNumber . #getApiT) slotNum  w
        expectEventually' ctx (#tip . #height) blockHeight w

    describe "NETWORK - v2/network/information - Methods Not Allowed" $ do
        let matrix = ["POST", "CONNECT", "TRACE", "OPTIONS"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            let endpoint = (method, "v2/network/information")
            r <- request @ApiNetworkInformation ctx endpoint Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    describe "NETWORK - HTTP headers" $ do
        let matrix =
                  [ ( "No HTTP headers -> 200", None
                    , [ expectResponseCode @IO HTTP.status200 ] )
                  , ( "Accept: text/plain -> 406"
                    , Headers
                          [ ("Content-Type", "application/json")
                          , ("Accept", "text/plain") ]
                    , [ expectResponseCode @IO HTTP.status406
                      , expectErrorMessage errMsg406 ]
                    )
                  , ( "No Accept -> 200"
                    , Headers [ ("Content-Type", "application/json") ]
                    , [ expectResponseCode @IO HTTP.status200 ]
                    )
                  , ( "No Content-Type -> 200"
                    , Headers [ ("Accept", "application/json") ]
                    , [ expectResponseCode @IO HTTP.status200 ]
                    )
                  , ( "Content-Type: text/plain -> 200"
                    , Headers [ ("Content-Type", "text/plain") ]
                    , [ expectResponseCode @IO HTTP.status200 ]
                    )
                  ]
        forM_ matrix $ \(title, headers, expectations) -> it title $ \ctx -> do
            r <- request @ApiNetworkInformation ctx networkInfoEp headers Empty
            verify r expectations
