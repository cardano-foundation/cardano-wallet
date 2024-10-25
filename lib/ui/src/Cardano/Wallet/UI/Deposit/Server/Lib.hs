module Cardano.Wallet.UI.Deposit.Server.Lib
    ( alert
    , origin
    , renderSmoothHtml
    )
where

import Prelude

import Cardano.Wallet.Deposit.Time
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

alert :: ToHtml a => a -> RawHtml
alert = renderHtml . alertH

renderSmoothHtml :: Html () -> RawHtml
renderSmoothHtml response =
    renderHtml
        $ div_ [class_ "smooth"]
        $ toHtml response

origin :: UTCTime
origin = posixSecondsToUTCTime $ fromIntegral originTime
