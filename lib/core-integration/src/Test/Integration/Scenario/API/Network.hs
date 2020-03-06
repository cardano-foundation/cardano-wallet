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
    , ApiNetworkClock
    , ApiNetworkInformation
    , ApiNetworkParameters (..)
    , ApiT (..)
    , NtpSyncingStatus (..)
    , WalletStyle (..)
    , toApiNetworkParameters
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..), SyncProgress (..) )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
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
    , eventually
    , eventuallyUsingDelay
    , expectErrorMessage
    , expectField
    , expectResponseCode
    , getFromResponse
    , request
    , verify
    , waitForNextEpoch
    , (.>)
    )
import Test.Integration.Framework.TestData
    ( errMsg404NoEpochNo )
import Test.Utils.Windows
    ( pendingOnWindows )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t. SpecWith (Context t)
spec = do
    it "NETWORK - Can query network information" $ \ctx -> do
        r <- eventually "wallet's syncProgress = Ready" $ do
            now <- liftIO getCurrentTime
            r <- request @ApiNetworkInformation ctx
                Link.getNetworkInfo Default Empty
            expectResponseCode @IO HTTP.status200 r
            verify r
                [ expectField #nextEpoch ((.> now) . epochStartTime)
                , expectField (#syncProgress . #getApiT) (`shouldBe` Ready)
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

        eventuallyUsingDelay 100 "nextEpochStartTime passes" $ do
            now <- liftIO getCurrentTime
            now `shouldSatisfy` (>= nextEpochStartTime)

        r2 <- request @ApiNetworkInformation ctx
            Link.getNetworkInfo Default Empty
        let currentEpoch = getFromResponse (#networkTip . #epochNumber) r2
        currentEpoch `shouldBe` calculatedNextEpoch

    it "NETWORK_BYRON - Byron wallet has the same tip as network/information" $
        \ctx -> do
            let getNetworkInfo = request @ApiNetworkInformation ctx
                    Link.getNetworkInfo Default Empty
            w <- emptyRandomWallet ctx
            waitForNextEpoch ctx
            r <- eventually "Network info enpoint shows syncProgress = Ready" $ do
                sync <- getNetworkInfo
                expectField (#syncProgress . #getApiT) (`shouldBe` Ready) sync
                return sync

            let epochNum =
                    getFromResponse (#nodeTip . #epochNumber . #getApiT) r
            let slotNum =
                    getFromResponse (#nodeTip . #slotNumber . #getApiT) r
            let blockHeight =
                    getFromResponse (#nodeTip . #height) r

            eventually "Wallet has the same tip as network/information" $ do
                res <- request @ApiByronWallet ctx
                    (Link.getWallet @'Byron w) Default Empty
                verify res
                    [ expectField (#state . #getApiT) (`shouldBe` Ready)
                    , expectField (#tip . #epochNumber . #getApiT) (`shouldBe` epochNum)
                    , expectField (#tip . #slotNumber  . #getApiT) (`shouldBe` slotNum)
                    , expectField (#tip . #height) (`shouldBe` blockHeight)
                    ]

    describe "NETWORK_PARAMS_01 - Valid epoch values" $ do
        let matrix = ["latest", "0"]
        forM_ matrix $ \epochNo -> it ("Epoch: " <> show epochNo) $ \ctx -> do
            verifyEpochNumOK ctx epochNo

        it "Current epoch" $ \ctx -> do
            r <- request @ApiNetworkInformation ctx
                Link.getNetworkInfo Default Empty
            let currentEpochNo =
                    getFromResponse
                        (#nodeTip . #epochNumber . #getApiT . #unEpochNo) r
            let epochNo = T.pack $ show currentEpochNo
            verifyEpochNumOK ctx epochNo

        it "Previous epoch" $ \ctx -> do
            r <- request @ApiNetworkInformation ctx
                Link.getNetworkInfo Default Empty
            let currentEpochNo =
                    getFromResponse
                        (#nodeTip . #epochNumber . #getApiT . #unEpochNo) r
            -- test previous epoch unless the current is epoch 0
            -- otherwise test hits maxBound of the epochNo and fails
            epochNo <- if (currentEpochNo > 0) then do
                return $ T.pack $ show (currentEpochNo - 1)
            else do
                return $ T.pack $ show currentEpochNo

            verifyEpochNumOK ctx epochNo

    describe "NETWORK_PARAMS_02 - Cannot query future epoch" $  do
        it "Future epoch" $ \ctx -> do
            r1 <- request @ApiNetworkInformation ctx
                Link.getNetworkInfo Default Empty
            let (ApiT (EpochNo currentEpochNo)) =
                    getFromResponse (#nextEpoch . #epochNumber) r1
            let futureEpochNo = T.pack $ show $ currentEpochNo + 10
            verifyEpochNumWrong ctx
                    futureEpochNo
                    HTTP.status404
                    (errMsg404NoEpochNo (T.unpack futureEpochNo))

        it "Epoch max value" $ \ctx -> do
            let maxEpochValue =
                    T.pack $ show $ fromIntegral @Word31 @Int maxBound
            verifyEpochNumWrong ctx
                    maxEpochValue
                    HTTP.status404
                    (errMsg404NoEpochNo (T.unpack maxEpochValue))

    it "NETWORK_CLOCK - Can query network clock" $ \ctx -> do
        pendingOnWindows "network/clock at this point is not supported on Windows"
        eventually "ntp status = (un)available" $ do
            r <- request @ApiNetworkClock ctx
                Link.getNetworkClock Default Empty
            expectResponseCode @IO HTTP.status200 r
            expectField (#ntpStatus . #status)
                (`shouldBe` NtpSyncingStatusAvailable) r
   where
       verifyEpochNumWrong
            :: Context t
            -> T.Text
            -> HTTP.Status
            -> String
            -> IO()
       verifyEpochNumWrong ctx epochNum errCode errMsg = do
           let endpoint =
                   ( "GET", "v2/network/parameters/" <> epochNum )
           r <- request @ApiNetworkParameters ctx endpoint Default Empty
           expectResponseCode @IO errCode r
           expectErrorMessage errMsg r

       verifyEpochNumOK :: Context t -> T.Text -> IO ()
       verifyEpochNumOK ctx epochNo = do
           let endpoint = ( "GET", "v2/network/parameters/"<>epochNo )
           r2 <- request @ApiNetworkParameters ctx endpoint Default Empty
           expectResponseCode @IO HTTP.status200 r2
           let networkParams = getFromResponse id r2
           networkParams `shouldBe`
               toApiNetworkParameters (ctx ^. #_blockchainParameters)
