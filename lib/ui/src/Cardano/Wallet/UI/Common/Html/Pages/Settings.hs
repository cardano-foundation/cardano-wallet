module Cardano.Wallet.UI.Common.Html.Pages.Settings where

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

import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( record
    , simpleField
    , sseH
    )
import Cardano.Wallet.UI.Common.Layer
    ( State
    , sseEnabled
    )
import Control.Lens
    ( view
    )
import Servant.Links
    ( Link
    )

-- | Settings page
settingsPageH :: Link -> Link -> Html ()
settingsPageH sseLink settingsGetLink =
    sseH sseLink settingsGetLink "content" ["settings"]

-- | Settings state
settingsStateH :: Link -> State s -> Html ()
settingsStateH settingsSseToggleLink state =
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
