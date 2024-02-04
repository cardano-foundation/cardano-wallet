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
    ( linkText
    , settingsGetLink
    , settingsSseToggleLink
    , sseLink
    )
import Cardano.Wallet.UI.Html.Pages.Lib
    ( record
    , simpleField
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
settingsPageH = sseH sseLink settingsGetLink "content" ["settings"]

settingsStateH :: State -> Html ()
settingsStateH state =
    record $ do
        simpleField "Auto refresh" $ do
            input_
                $ [ hxTrigger_ "click"
                  , type_ "checkbox"
                  , class_ "form-check-input"
                  , hxPost_ (linkText settingsSseToggleLink)
                  , hxSwap_ "none"
                  ]
                    <> [checked_ | view refreshEnabled state]
