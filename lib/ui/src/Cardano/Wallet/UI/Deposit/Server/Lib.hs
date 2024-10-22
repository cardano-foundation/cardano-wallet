module Cardano.Wallet.UI.Deposit.Server.Lib
    ( alert
    , origin
    , renderSmoothHtml
    , showTime
    )
where

import Prelude

import Cardano.Wallet.Deposit.IO.Network.Mock
    ( originTime
    )
import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    , renderHtml
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( alertH
    )
import Data.Time
    ( UTCTime
    , defaultTimeLocale
    , formatTime
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import Lucid
    ( Html
    , ToHtml (..)
    , class_
    , div_
    )

showTime :: UTCTime -> String
showTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

alert :: ToHtml a => a -> RawHtml
alert = renderHtml . alertH

renderSmoothHtml :: Html () -> RawHtml
renderSmoothHtml response =
    renderHtml
        $ div_ [class_ "smooth"]
        $ toHtml response

origin :: UTCTime
origin = posixSecondsToUTCTime $ fromIntegral originTime
