{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.API.Network
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiEpochInfo (..)
    , ApiNetworkInformation
    , ApiNetworkParameters
    , ApiT (..)
    , ApiWallet
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..), SyncProgress (..) )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Time.Clock
    ( getCurrentTime )
import Data.Word.Odd
    ( Word31 )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe, shouldSatisfy )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , emptyRandomWallet
    , emptyWallet
    , eventually
    , eventuallyUsingDelay
    , eventually_
    , expectErrorMessage
    , expectFieldEqual
    , expectFieldSatisfy
    , expectResponseCode
    , getFromResponse
    , request
    , verify
    )
import Test.Integration.Framework.TestData
    ( errMsg400MalformedEpoch, errMsg404NoEpochNo, errMsg405, getHeaderCases )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t. SpecWith (Context t)
spec = do
    it "NETWORK - Can query network information" $ \ctx -> do
        r <- eventually $ do
            now <- liftIO getCurrentTime
            r <- request @ApiNetworkInformation ctx
                Link.getNetworkInfo Default Empty
            expectResponseCode @IO HTTP.status200 r
            verify r
                [ expectFieldSatisfy #nextEpoch ((> now) . epochStartTime)
                , expectFieldEqual (#syncProgress . #getApiT) Ready
                ]
            return r

        let currentEpochNum =
                getFromResponse (#networkTip . #epochNumber . #getApiT) r
        let nextEpochNum =
                getFromResponse (#nextEpoch . #epochNumber . #getApiT) r
        nextEpochNum `shouldBe` currentEpochNum + 1

    it "NETWORK - Calculated next epoch is the next epoch" $ \ctx -> do
        r1 <- request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty
        let calculatedNextEpoch = getFromResponse (#nextEpoch . #epochNumber) r1
        let nextEpochStartTime = getFromResponse (#nextEpoch . #epochStartTime) r1

        eventuallyUsingDelay 100 $ do
            now <- liftIO getCurrentTime
            now `shouldSatisfy` (>= nextEpochStartTime)

        r2 <- request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty
        let currentEpoch = getFromResponse (#networkTip . #epochNumber) r2
        currentEpoch `shouldBe` calculatedNextEpoch

    it "NETWORK_SHELLEY - Wallet has the same tip as network/information" $
        \ctx -> do
            let getNetworkInfo = request @ApiNetworkInformation ctx
                    Link.getNetworkInfo Default Empty
            w <- emptyWallet ctx
            eventually_ $ do
                sync <- getNetworkInfo
                verify sync [ expectFieldEqual (#syncProgress . #getApiT) Ready ]
            r <- getNetworkInfo

            let epochNum =
                    getFromResponse (#nodeTip . #epochNumber . #getApiT) r
            let slotNum =
                    getFromResponse (#nodeTip . #slotNumber . #getApiT) r
            let blockHeight =
                    getFromResponse (#nodeTip . #height) r

            eventually $ do
                res <- request @ApiWallet ctx
                    (Link.getWallet @'Shelley w) Default Empty
                verify res
                    [ expectFieldEqual (#state . #getApiT) Ready
                    , expectFieldEqual (#tip . #epochNumber . #getApiT) epochNum
                    , expectFieldEqual (#tip . #slotNumber  . #getApiT) slotNum
                    , expectFieldEqual (#tip . #height) blockHeight
                    ]

    it "NETWORK_BYRON - Byron wallet has the same tip as network/information" $
        \ctx -> do
            let getNetworkInfo = request @ApiNetworkInformation ctx
                    Link.getNetworkInfo Default Empty
            w <- emptyRandomWallet ctx
            eventually_ $ do
                sync <- getNetworkInfo
                verify sync [ expectFieldEqual (#syncProgress . #getApiT) Ready ]
            r <- getNetworkInfo

            let epochNum =
                    getFromResponse (#nodeTip . #epochNumber . #getApiT) r
            let slotNum =
                    getFromResponse (#nodeTip . #slotNumber . #getApiT) r
            let blockHeight =
                    getFromResponse (#nodeTip . #height) r

            eventually $ do
                res <- request @ApiByronWallet ctx
                    (Link.getWallet @'Byron w) Default Empty
                verify res
                    [ expectFieldEqual (#state . #getApiT) Ready
                    , expectFieldEqual (#tip . #epochNumber . #getApiT) epochNum
                    , expectFieldEqual (#tip . #slotNumber  . #getApiT) slotNum
                    , expectFieldEqual (#tip . #height) blockHeight
                    ]

    describe "NETWORK - v2/network/information - Methods Not Allowed" $ do
        let matrix = ["POST", "CONNECT", "TRACE", "OPTIONS"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            let endpoint = (method, "v2/network/information")
            r <- request @ApiNetworkInformation ctx endpoint Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    describe "NETWORK - HTTP headers" $ do
        forM_ (getHeaderCases HTTP.status200)
            $ \(title, headers, expectations) -> it title $ \ctx -> do
                r <- request @ApiNetworkInformation ctx
                    Link.getNetworkInfo headers Empty
                verify r expectations

    describe "NETWORK - Cannot query blockchain parameters with \
             \invalid arguments" $ do
        let epochNoWrong =
                T.pack $show $ (+10) $ fromIntegral @Word31 @Int maxBound
        let matrix = ["earliest", "invalid", epochNoWrong, "-1"]
        forM_ matrix $ \arg -> it (show arg) $ \ctx -> do
            let endpoint = ( "GET", "v2/network/parameters/"<>arg )
            r <- request @ApiNetworkParameters ctx endpoint Default Empty
            expectResponseCode @IO HTTP.status400 r
            expectErrorMessage (errMsg400MalformedEpoch $ T.unpack arg) r

    describe "NETWORK - v2/network/parameters - Methods Not Allowed" $ do
        let matrix = ["POST", "CONNECT", "TRACE", "OPTIONS"]
        forM_ matrix $ \method -> it (show method) $ \ctx -> do
            let endpoint = (method, "v2/network/parameters/latest")
            r <- request @ApiNetworkParameters ctx endpoint Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    describe "NETWORK - HTTP headers" $ do
        forM_ (getHeaderCases HTTP.status200)
            $ \(title, headers, expectations) -> it title $ \ctx -> do
                r <- request @ApiNetworkParameters ctx
                    Link.getNetworkInfo headers Empty
                verify r expectations

    it "NETWORK - Cannot query blockchain parameters with epoch later \
       \than current one" $ \ctx -> do
        r1 <- request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty
        let (ApiT (EpochNo currentEpochNo)) =
                getFromResponse (#nextEpoch . #epochNumber) r1

        let futureEpochNo = T.pack $ show $ currentEpochNo + 10
        let endpoint = ( "GET", "v2/network/parameters/"<>futureEpochNo )
        r2 <- request @ApiNetworkParameters ctx endpoint Default Empty

        expectResponseCode @IO HTTP.status404 r2
        expectErrorMessage (errMsg404NoEpochNo (T.unpack futureEpochNo)) r2

    describe "NETWORK - Can query blockchain parameters with \
             \valid arguments" $ do
        let matrix = ["latest", "0"]
        forM_ matrix $ \arg -> it (show arg) $ \ctx -> do
            let endpoint = ( "GET", "v2/network/parameters/"<>arg )
            r <- request @ApiNetworkParameters ctx endpoint Default Empty
            expectResponseCode @IO HTTP.status200 r

    it "NETWORK - Can query blockchain parameters with \
             \valid arguments" $ \ctx -> do
        r <- request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty
        let (ApiT (EpochNo currentEpochNo)) =
                getFromResponse (#nextEpoch . #epochNumber) r

        let epochNoOk1 = T.pack $ show $ currentEpochNo - 10
        let endpoint1 = ( "GET", "v2/network/parameters/"<>epochNoOk1 )
        r1@(_,(Right apiParams1)) <-
            request @ApiNetworkParameters ctx endpoint1 Default Empty
        expectResponseCode @IO HTTP.status200 r1

        let epochNoOk2 = T.pack $ show $ currentEpochNo - 1
        let endpoint2 = ( "GET", "v2/network/parameters/"<>epochNoOk2 )
        r2@(_,(Right apiParams2)) <- request @ApiNetworkParameters ctx endpoint2 Default Empty
        expectResponseCode @IO HTTP.status200 r2

        apiParams1 `shouldBe` apiParams2
