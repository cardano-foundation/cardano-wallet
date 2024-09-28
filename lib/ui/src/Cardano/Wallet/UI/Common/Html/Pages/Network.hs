{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Common.Html.Pages.Network where

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
import Cardano.Wallet.UI.Common.Html.Lib
    ( ShowTime
    , showHtml
    , showPercentage
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
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
    , HtmlT
    , ToHtml (..)
    , p_
    )
import Servant.Links
    ( Link
    )

import qualified Data.Percentage as Percentage

-- | Network information tag
networkH
    :: Monad m
    => Link
    -- ^ Link to the network information endpoint
    -> HtmlT m ()
networkH networkInfoLink =
    sseH networkInfoLink "content" ["tip"]

-- | Render the network information as a record
networkInfoH
    :: ShowTime
    -- ^ how to show time
    -> ApiNetworkInformation
    -- ^ network information
    -> Html ()
networkInfoH showTime ApiNetworkInformation{..} = record (Just 7) $ do
    simpleField "Sync progress" $ syncProgressH progress
    simpleField "Next epoch" $ nextEpochH nextEpoch
    simpleField "Node tip" $ blockReferenceH showTime nodeTip
    simpleField "Node era" $ nodeEraH nodeEra
    simpleField "Network tip" $ networkTipH showTime networkTip
    simpleField "Network info" $ networkIdH networkInfo
  where
    ApiT progress = syncProgress

-- | Render the next epoch information as a record
nextEpochH :: Maybe EpochInfo -> Html ()
nextEpochH Nothing = p_ "Unknown"
nextEpochH (Just EpochInfo{..}) = do
    record (Just 5) $ do
        simpleField "Epoch Start" $ showHtml epochStartTime
        simpleField "Epoch Number" $ toHtml $ showThousandDots epochNumber'
  where
    EpochNo epochNumber' = epochNumber

-- | Render the sync progress
syncProgressH :: SyncProgress -> Html ()
syncProgressH Ready = "Ready"
syncProgressH (Syncing (Quantity percentage)) =
    "Syncing " <> toHtml (showPercentage $ Percentage.toRational percentage)
syncProgressH (NotResponding) = "Not Responding"

-- | Render a block reference as a record
blockReferenceH :: ShowTime -> ApiBlockReference -> Html ()
blockReferenceH showTime ApiBlockReference{..} =
    record (Just 4) $ do
        simpleField "Slot" $ toHtml $ showThousandDots slot
        simpleField "Time" $ toHtml $ showTime time
        simpleField "Block" $ blockInfoH block
  where
    ApiT (SlotNo slot) = absoluteSlotNumber

-- | Render a block info
blockInfoH :: ApiBlockInfo -> Html ()
blockInfoH (ApiBlockInfo (Quantity height)) = toHtml (showThousandDots height)

-- | Render a network tip as a record
networkTipH :: ShowTime -> Maybe ApiSlotReference -> Html ()
networkTipH _ Nothing = "Unknown"
networkTipH showTime (Just ApiSlotReference{..}) = do
    record (Just 4) $ do
        simpleField "Slot" $ toHtml $ showThousandDots slot
        simpleField "Time" $ toHtml $ showTime time
  where
    ApiT (SlotNo slot) = absoluteSlotNumber

-- | Render a node era
nodeEraH :: ApiEra -> Html ()
nodeEraH ApiByron = "Byron"
nodeEraH ApiShelley = "Shelley"
nodeEraH ApiAllegra = "Allegra"
nodeEraH ApiMary = "Mary"
nodeEraH ApiAlonzo = "Alonzo"
nodeEraH ApiBabbage = "Babbage"
nodeEraH ApiConway = "Conway"

-- | Render the network id and protocol magic as a record
networkIdH :: ApiNetworkInfo -> Html ()
networkIdH ApiNetworkInfo{..} = do
    record (Just 11) $ do
        simpleField "Network ID" $ toHtml networkId
        fieldShow [] "Protocol Magic" protocolMagic
