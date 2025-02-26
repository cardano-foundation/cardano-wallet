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
    , lovelaceH
    , modalElementH
    , chainPointToSlotH
    , networkTagH
    , addressH
    )
where

import Prelude

import Cardano.Wallet.Deposit.Pure.API.Address
    ( encodeAddress
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( DownTime
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , NetworkTag (..)
    , Slot
    , TxId
    , WithOrigin (..)
    )
import Cardano.Wallet.Read
    ( ChainPoint (..)
    , Coin (..)
    , SlotNo (..)
    , Value (..)
    , hashFromTxId
    )
import Cardano.Wallet.Read.Hash
    ( hashToStringAsHex
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( WithCopy (..)
    , dataBsDismiss_
    , truncatableText
    )
import Cardano.Wallet.UI.Common.Html.Modal
    ( ModalData (..)
    , mkModal
    )
import Data.Ord
    ( Down (..)
    )
import Data.Text
    ( Text
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
    , HtmlT
    , ToHtml (..)
    , button_
    , class_
    , span_
    )
import Numeric
    ( showFFloatAlt
    )
import Numeric.Natural
    ( Natural
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

chainPointToSlotH
    :: ChainPoint
    -> Html ()
chainPointToSlotH cp = case cp of
    GenesisPoint -> toHtml ("Genesis" :: Text)
    BlockPoint (SlotNo n) _ -> toHtml $ show n

networkTagH :: NetworkTag -> Html ()
networkTagH = toHtml . showTag

showTag :: NetworkTag -> Text
showTag MainnetTag = "Mainnet"
showTag TestnetTag = "Testnet"

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
valueH (ValueC (CoinC c) _) = lovelaceH $ fromIntegral c

lovelaceH :: Natural -> Html ()
lovelaceH c = do
    span_ $ toHtml $ showLovelaceAsAda c
    span_ [class_ "opacity-25"] "₳"

showLovelaceAsAda :: Integral a => a -> String
showLovelaceAsAda c =
    showFFloatAlt @Double (Just 2) (fromIntegral c / 1_000_000) ""

modalElementH :: Maybe Text -> Maybe Text -> Html ()
modalElementH (Just t) (Just b) =
    mkModal
        $ ModalData
            { modalTitle = toHtml t
            , modalBody = toHtml b
            , modalFooter =
                button_
                    [ class_ "btn btn-secondary"
                    , dataBsDismiss_ "modal"
                    ]
                    "Dismiss"
            }
modalElementH _ _ = mempty

addressH :: Monad m => WithCopy -> Address -> HtmlT m ()
addressH copy addr =
    truncatableText copy ("address-text-" <> encodedAddr)
        $ toHtml encodedAddr
  where
    encodedAddr = encodeAddress addr
