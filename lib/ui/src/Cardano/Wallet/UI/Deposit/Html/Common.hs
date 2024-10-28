{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Deposit.Html.Common
    ( downTimeH
    , timeH
    , slotH
    , txIdH
    , showTime
    , showTimeSecs
    , withOriginH
    , valueH
    )
where

import Prelude

import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( DownTime
    )
import Cardano.Wallet.Deposit.Read
    ( Slot
    , TxId
    , WithOrigin (..)
    )
import Cardano.Wallet.Read
    ( Coin (..)
    , SlotNo (..)
    , Value (..)
    , hashFromTxId
    )
import Cardano.Wallet.Read.Hash
    ( hashToStringAsHex
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( WithCopy (..)
    , truncatableText
    )
import Data.Ord
    ( Down (..)
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , formatTime
    )
import Lucid
    ( Html
    , ToHtml (..)
    , class_
    , span_
    )
import Numeric
    ( showFFloatAlt
    )

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

showTimeSecs :: UTCTime -> String
showTimeSecs = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

withOriginH :: (a -> Html ()) -> WithOrigin a -> Html ()
withOriginH f = \case
    Origin -> "Origin"
    At a -> f a

timeH :: UTCTime -> Html ()
timeH = toHtml . showTime

downTimeH :: DownTime -> Html ()
downTimeH (Down time) = withOriginH timeH time

slotH :: Slot -> Html ()
slotH = \case
    Origin -> "Origin"
    At (SlotNo s) -> toHtml $ show s

txIdH :: TxId -> Html ()
txIdH txId =
    truncatableText WithCopy ("tx-id-text-" <> toText (take 16 h))
        $ toHtml h
  where
    h =
        hashToStringAsHex
            $ hashFromTxId
                txId

valueH :: Value -> Html ()
valueH (ValueC (CoinC c) _) = do
    span_ $ toHtml $ a ""
    span_ [class_ "opacity-25"] "₳"
  where
    a = showFFloatAlt @Double (Just 2) $ fromIntegral c / 1_000_000
