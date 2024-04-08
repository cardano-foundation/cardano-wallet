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
    , sseLink
    )
import Cardano.Wallet.UI.Html.Lib
    ( ShowTime
    , showPercentage
    )
import Cardano.Wallet.UI.Html.Pages.Lib
    ( fieldShow
    , record
    , showThousandDots
    , simpleField
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
networkH = sseH sseLink networkInfoLink "content" ["refresh"]

networkInfoH :: ShowTime -> ApiNetworkInformation -> Html ()
networkInfoH showTime ApiNetworkInformation{..} = record $ do
    simpleField "Sync progress" $ syncProgressH progress
    simpleField "Next epoch" $ nextEpochH nextEpoch
    simpleField "Node tip" $ blockReferenceH showTime nodeTip
    simpleField "Node era" $ nodeEraH nodeEra
    simpleField "Network tip" $ networkTipH showTime networkTip
    simpleField "Network info" $ networkInfoH' networkInfo
  where
    ApiT progress = syncProgress

nextEpochH :: Maybe EpochInfo -> Html ()
nextEpochH Nothing = p_ "Unknown"
nextEpochH (Just EpochInfo{..}) = do
    record $ do
        simpleField "Epoch start" $ show epochStartTime
        simpleField "Epoch number" $ showThousandDots epochNumber'
  where
    EpochNo epochNumber' = epochNumber

syncProgressH :: SyncProgress -> Html ()
syncProgressH Ready = "Ready"
syncProgressH (Syncing (Quantity percentage)) =
    "Syncing " <> toHtml (showPercentage $ Percentage.toRational percentage)
syncProgressH (NotResponding) = "Not Responding"

blockReferenceH :: ShowTime -> ApiBlockReference -> Html ()
blockReferenceH showTime ApiBlockReference{..} =
    record $ do
        simpleField "Slot" $ showThousandDots slot
        simpleField "Time" $ showTime time
        simpleField "Block" $ blockInfoH block
  where
    ApiT (SlotNo slot) = absoluteSlotNumber

blockInfoH :: ApiBlockInfo -> Html ()
blockInfoH (ApiBlockInfo (Quantity height)) = toHtml (showThousandDots height)

networkTipH :: ShowTime ->  Maybe ApiSlotReference -> Html ()
networkTipH _ Nothing = "Unknown"
networkTipH showTime (Just ApiSlotReference{..}) = do
    record $ do
        simpleField "Slot" $ showThousandDots slot
        simpleField "Time" $ showTime time
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
    record $ do
        simpleField "Network ID" networkId
        fieldShow [] "Protocol Magic" protocolMagic
