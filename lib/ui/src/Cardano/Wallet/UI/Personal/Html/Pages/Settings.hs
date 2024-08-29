module Cardano.Wallet.UI.Personal.Html.Pages.Settings where

import Prelude hiding
    ( id
    )

import Cardano.Wallet.UI.Common.Html.Htmx
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

import Cardano.Wallet.UI.Personal.API
    ( linkText
    , settingsGetLink
    , settingsSseToggleLink
    , sseLink
    )
import Cardano.Wallet.UI.Personal.Html.Pages.Lib
    ( record
    , simpleField
    , sseH
    )
import Cardano.Wallet.UI.Personal.Layer
    ( State
    , sseEnabled
    )
import Control.Lens
    ( view
    )

settingsPageH :: Html ()
settingsPageH = sseH sseLink settingsGetLink "content" ["settings"]

settingsStateH :: State s -> Html ()
settingsStateH state =
    record $ do
        simpleField "Enable SSE" $ do
            input_
                $ [ hxTrigger_ "click"
                  , type_ "checkbox"
                  , class_ "form-check-input"
                  , hxPost_ (linkText settingsSseToggleLink)
                  , hxSwap_ "none"
                  ]
                    <> [checked_ | view sseEnabled state]
