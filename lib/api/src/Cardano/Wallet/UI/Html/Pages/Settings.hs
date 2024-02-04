module Cardano.Wallet.UI.Html.Pages.Settings where

import Prelude hiding
    ( id
    )

import Cardano.Wallet.UI.Html.Htmx
    ( hxPost_
    , hxSwap_
    , hxTrigger_
    )
import Lucid
    ( Html
    , checked_
    , class_
    , input_
    , type_
    )

import Cardano.Wallet.UI.API
    ( settingsGetLink
    )
import Cardano.Wallet.UI.Html.Pages.Lib
    ( recordTable
    , row
    , sseH
    )
import Cardano.Wallet.UI.Layer
    ( State
    , refreshEnabled
    )
import Control.Lens
    ( view
    )

settingsPageH :: Html ()
settingsPageH = sseH settingsGetLink "content" ["settings"]

settingsStateH :: State -> Html ()
settingsStateH state =
    recordTable $ do
        row [] "Auto refresh" $ do
            input_
                $ [ hxTrigger_ "click"
                  , type_ "checkbox"
                  , class_ "form-check-input"
                  , hxPost_ "/settings/sse/toggle"
                  , hxSwap_ "none"
                  ]
                    <> [checked_ | view refreshEnabled state]
