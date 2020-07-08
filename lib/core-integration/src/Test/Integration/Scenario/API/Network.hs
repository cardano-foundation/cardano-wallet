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
    , NtpSyncingStatus (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Control.Monad
    ( when )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Time.Clock
    ( getCurrentTime )
import Test.Hspec
    ( SpecWith, it, pendingWith, shouldBe )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , emptyRandomWallet
    , eventually
    , expectField
    , expectResponseCode
    , getFromResponse
    , request
    , verify
    , (.>)
    )
import Test.Utils.Paths
    ( inNixBuild )

import qualified Cardano.Wallet.Api.Link as Link
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

    it "NETWORK_BYRON - Byron wallet has the same tip as network/information" $
        \ctx -> do
            let getNetworkInfo = request @ApiNetworkInformation ctx
                    Link.getNetworkInfo Default Empty
            w <- emptyRandomWallet ctx
            eventually "Wallet has the same tip as network/information" $ do
                sync <- getNetworkInfo
                expectField (#syncProgress . #getApiT) (`shouldBe` Ready) sync

                let epochNum =
                        getFromResponse (#nodeTip . #epochNumber . #getApiT) sync
                let slotNum =
                        getFromResponse (#nodeTip . #slotNumber . #getApiT) sync
                let blockHeight =
                        getFromResponse (#nodeTip . #height) sync

                res <- request @ApiByronWallet ctx
                    (Link.getWallet @'Byron w) Default Empty
                verify res
                    [ expectField (#state . #getApiT) (`shouldBe` Ready)
                    , expectField (#tip . #epochNumber . #getApiT) (`shouldBe` epochNum)
                    , expectField (#tip . #slotNumber  . #getApiT) (`shouldBe` slotNum)
                    , expectField (#tip . #height) (`shouldBe` blockHeight)
                    ]

    it "NETWORK_CLOCK - Can query network clock" $ \ctx -> do
        sandboxed <- inNixBuild
        when sandboxed $
            pendingWith "Internet NTP servers unavailable in build sandbox"
        eventually "ntp status = (un)available" $ do
            r <- request @ApiNetworkClock ctx
                Link.getNetworkClock Default Empty
            expectResponseCode @IO HTTP.status200 r
            expectField (#ntpStatus . #status)
                (`shouldBe` NtpSyncingStatusAvailable) r

    it "NETWORK_CLOCK - Can query network clock and force NTP check" $ \ctx -> do
        sandboxed <- inNixBuild
        when sandboxed $
            pendingWith "Internet NTP servers unavailable in build sandbox"
        eventually "ntp status = (un)available" $ do
            r <- request @ApiNetworkClock ctx
                (Link.getNetworkClock' True) Default Empty
            expectResponseCode @IO HTTP.status200 r
            expectField (#ntpStatus . #status)
                (`shouldBe` NtpSyncingStatusAvailable) r
