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
    , HtmlT
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
settingsPageH :: Monad m => Link -> HtmlT m ()
settingsPageH settingsGetLink =
    sseH settingsGetLink "content" ["settings"]

-- | Settings state
settingsStateH :: Link -> State s -> Html ()
settingsStateH settingsSseToggleLink state =
    record Nothing $ do
        simpleField "Enable SSE" $ do
            input_
                $ [ hxTrigger_ "click"
                  , type_ "checkbox"
                  , class_ "form-check-input"
                  , hxPost_ (linkText settingsSseToggleLink)
                  , hxSwap_ "none"
                  ]
                    <> [checked_ | view sseEnabled state]
