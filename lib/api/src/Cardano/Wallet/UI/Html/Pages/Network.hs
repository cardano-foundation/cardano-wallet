{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Html.Pages.Network where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Cardano.Wallet.Api.Types
    ( ApiBlockInfo (..)
    , ApiBlockReference (..)
    , ApiEra (..)
    , ApiNetworkInfo (..)
    , ApiNetworkInformation (..)
    , ApiSlotReference (..)
    , ApiT (..)
    )
import Cardano.Wallet.Pools
    ( EpochInfo (..)
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..)
    )
import Cardano.Wallet.Primitive.Types.EpochNo
    ( EpochNo (..)
    )
import Cardano.Wallet.UI.API
    ( networkInfoLink
    )
import Cardano.Wallet.UI.Html.Lib
    ( showPercentage
    )
import Cardano.Wallet.UI.Html.Pages.Lib
    ( recordTable
    , row
    , rowShow
    , showThousandDots
    , sseH
    )
import Data.Quantity
    ( Quantity (..)
    )
import Lucid
    ( Html
    , ToHtml (..)
    , p_
    )

import qualified Data.Percentage as Percentage

networkH :: Html ()
networkH = sseH networkInfoLink "content" ["refresh"]

networkInfoH :: ApiNetworkInformation -> Html ()
networkInfoH ApiNetworkInformation{..} = recordTable $ do
    row [] "Sync progress" $ syncProgressH progress
    row [] "Next epoch" $ nextEpochH nextEpoch
    row [] "Node tip" $ blockReferenceH nodeTip
    row [] "Node era" $ nodeEraH nodeEra
    row [] "Network tip" $ networkTipH networkTip
    row [] "Network info" $ networkInfoH' networkInfo
  where
    ApiT progress = syncProgress

nextEpochH :: Maybe EpochInfo -> Html ()
nextEpochH Nothing = p_ "Unknown"
nextEpochH (Just EpochInfo{..}) = do
    recordTable $ do
        rowShow [] "Epoch start" epochStartTime
        row [] "Epoch number" $ showThousandDots epochNumber'
  where
    EpochNo epochNumber' = epochNumber

syncProgressH :: SyncProgress -> Html ()
syncProgressH Ready = "Ready"
syncProgressH (Syncing (Quantity percentage)) =
    "Syncing " <> toHtml (showPercentage $ Percentage.toRational percentage)
syncProgressH (NotResponding) = "Not Responding"

blockReferenceH :: ApiBlockReference -> Html ()
blockReferenceH ApiBlockReference{..} =
    recordTable $ do
        row [] "Slot" $ showThousandDots slot
        rowShow [] "Time" time
        row [] "Block" $ blockInfoH block
  where
    ApiT (SlotNo slot) = absoluteSlotNumber

blockInfoH :: ApiBlockInfo -> Html ()
blockInfoH (ApiBlockInfo (Quantity height)) = toHtml (showThousandDots height)

networkTipH :: Maybe ApiSlotReference -> Html ()
networkTipH Nothing = "Unknown"
networkTipH (Just ApiSlotReference{..}) = do
    recordTable $ do
        row [] "Slot" $ showThousandDots slot
        rowShow [] "Time" time
  where
    ApiT (SlotNo slot) = absoluteSlotNumber

nodeEraH :: ApiEra -> Html ()
nodeEraH ApiByron = "Byron"
nodeEraH ApiShelley = "Shelley"
nodeEraH ApiAllegra = "Allegra"
nodeEraH ApiMary = "Mary"
nodeEraH ApiAlonzo = "Alonzo"
nodeEraH ApiBabbage = "Babbage"
nodeEraH ApiConway = "Conway"

networkInfoH' :: ApiNetworkInfo -> Html ()
networkInfoH' ApiNetworkInfo{..} = do
    recordTable $ do
        row [] "Network ID" networkId
        rowShow [] "Protocol Magic" protocolMagic
