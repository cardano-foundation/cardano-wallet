{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Integration.Scenario.API.Network
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiByronWallet
    , ApiNetworkClock
    , ApiNetworkInformation
    , ApiWalletMode (..)
    , NtpSyncingStatus (..)
    , WalletStyle (..)
    , nextEpoch
    )
import Cardano.Wallet.Pools
    ( EpochInfo (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types.ProtocolMagic
    ( getProtocolMagic, mainnetMagic )
import Control.Monad
    ( when )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Resource
    ( runResourceT )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Time.Clock
    ( getCurrentTime )
import Test.Hspec
    ( SpecWith, describe, pendingWith, shouldBe, shouldNotBe )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , counterexample
    , emptyRandomWallet
    , eventually
    , expectField
    , expectResponseCode
    , getFromResponse
    , request
    , unsafeResponse
    , verify
    , (.>)
    )
import Test.Utils.Paths
    ( inNixBuild )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec :: SpecWith Context
spec = describe "COMMON_NETWORK" $ do
    it "NETWORK - Can query network information" $ \ctx -> do
        eventually "wallet's syncProgress = Ready" $ do
            now <- liftIO getCurrentTime
            r <- request @ApiNetworkInformation ctx
                Link.getNetworkInfo Default Empty
            expectResponseCode @IO HTTP.status200 r
            let i = getFromResponse id r
            verify r
                [ expectField (#syncProgress . #getApiT) (`shouldBe` Ready)
                , expectField (#nodeEra) (`shouldBe` _mainEra ctx)
                , expectField (#nodeTip . #absoluteSlotNumber . #getApiT) (`shouldNotBe` 0)
                , \x -> (epochStartTime <$> nextEpoch (unsafeResponse x)) .> Just now
                , expectField (#networkInfo . #protocolMagic)
                    (`shouldBe` fromIntegral (getProtocolMagic mainnetMagic))
                , expectField (#walletMode) (`shouldBe` Node)
                ]
            counterexample (show r) $ do
                (epochStartTime <$> nextEpoch i) .> Just now
                let Just currentEpochNum =
                        view (#slotId . #epochNumber . #getApiT) <$> (i ^. #networkTip)
                let Just nextEpochNum =
                        view #epochNumber <$> getFromResponse #nextEpoch r
                nextEpochNum `shouldBe` currentEpochNum + 1

    it "NETWORK_BYRON - Byron wallet has the same tip as network/information" $
        \ctx -> runResourceT @IO $ do
            let getNetworkInfo = request @ApiNetworkInformation ctx
                    Link.getNetworkInfo Default Empty
            w <- emptyRandomWallet ctx
            eventually "Wallet has the same tip as network/information" $ do
                sync <- getNetworkInfo
                expectField (#syncProgress . #getApiT) (`shouldBe` Ready) sync

                let epochNum =
                        getFromResponse (#nodeTip . #slotId . #epochNumber . #getApiT) sync
                let slotNum =
                        getFromResponse (#nodeTip . #slotId . #slotNumber . #getApiT) sync
                let blockHeight =
                        getFromResponse (#nodeTip . #block . #height) sync
                let absSlot =
                        getFromResponse (#nodeTip . #absoluteSlotNumber) sync

                res <- request @ApiByronWallet ctx
                    (Link.getWallet @'Byron w) Default Empty
                verify res
                    [ expectField (#state . #getApiT) (`shouldBe` Ready)
                    , expectField (#tip . #slotId . #epochNumber . #getApiT) (`shouldBe` epochNum)
                    , expectField (#tip . #slotId . #slotNumber  . #getApiT) (`shouldBe` slotNum)
                    , expectField (#tip . #block . #height) (`shouldBe` blockHeight)
                    , expectField (#tip . #absoluteSlotNumber) (`shouldBe` absSlot)
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
