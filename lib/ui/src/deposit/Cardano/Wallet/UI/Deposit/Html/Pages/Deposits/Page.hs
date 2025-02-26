{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.Page
    ( depositsH
    , depositsElementH
    )
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletPublicIdentity (..)
    )
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxInclude_
    , hxPost_
    , hxTarget_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( AlertH
    , linkText
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( Striped (..)
    , Width (..)
    , box
    , record
    , simpleField
    , sseH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( WalletPresent (..)
    , onWalletPresentH
    )
import Cardano.Wallet.UI.Lib.Discretization
    ( Window (..)
    )
import Cardano.Wallet.UI.Type
    ( WHtml
    )
import Control.Monad
    ( forM_
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time
    ( DayOfWeek (..)
    )
import Lucid
    ( Html
    , ToHtml (..)
    , button_
    , class_
    , data_
    , div_
    , i_
    , id_
    , input_
    , name_
    , option_
    , select_
    , selected_
    , style_
    , type_
    , value_
    )
import Servant
    ( Link
    , ToHttpApiData (toUrlPiece)
    )

depositsH :: Link -> WHtml ()
depositsH depositsLink = do
    sseH depositsLink "deposits-page" ["wallet"]

depositsViewControls :: Html ()
depositsViewControls =
    div_ [class_ "collapse", id_ "columns-control"] $ do
        record Nothing Auto NotStriped $ do
            simpleField "Week Start"
                $ div_
                    [ class_ "d-flex justify-content-end align-items-center form-check"
                    ]
                $ select_
                    [ class_ "form-select w-auto m-1 p-1"
                    , id_ "select-first-week-day"
                    , name_ "first-week-day"
                    , style_ "background-image: none"
                    ]
                $ forM_ [Sunday, Monday]
                $ \day -> do
                    let selected = case day of
                            Monday -> (selected_ "" :)
                            _ -> id
                    option_ (selected [value_ $ toUrlPiece day])
                        $ toHtml
                        $ show day
            simpleField "Slot"
                $ div_
                    [ class_ "d-flex justify-content-end align-items-center form-check"
                    ]
                $ input_
                    [ class_ "form-check-input"
                    , type_ "checkbox"
                    , id_ "toggle-slot"
                    , name_ "slot"
                    , value_ ""
                    ]
            simpleField "Window"
                $ div_
                    [ class_ "d-flex justify-content-end align-items-center form-check"
                    ]
                $ select_
                    [ class_ "form-select w-auto m-1 p-1"
                    , id_ "select-window"
                    , name_ "window"
                    , style_ "background-image: none"
                    ]
                $ forM_ [Minute5 .. Year]
                $ \window -> do
                    let selected = case window of
                            Day -> (selected_ "" :)
                            _ -> id
                    option_ (selected [value_ $ toUrlPiece window])
                        $ toHtml
                        $ toText window
            simpleField "Spent"
                $ div_
                    [ class_ "d-flex justify-content-end align-items-center form-check"
                    ]
                $ input_
                    [ class_ "form-check-input"
                    , type_ "checkbox"
                    , id_ "toggle-spent"
                    , name_ "spent"
                    , value_ ""
                    ]

depositsElementH
    :: Link
    -> AlertH
    -> WalletPresent
    -> Html ()
depositsElementH depositsHistoryLink = onWalletPresentH $ \case
    WalletPublicIdentity _xpub _customers ->
        div_
            [ class_ "row mt-3 gx-0"
            ]
            $ do
                let configure = do
                        div_ [class_ "d-flex justify-content-end"] $ do
                            let toggle =
                                    button_
                                        [ class_ "btn p-0"
                                        , type_ "button"
                                        , data_ "bs-toggle" "collapse"
                                        , data_ "bs-target" "#columns-control"
                                        ]
                                        $ do
                                            i_ [class_ "bi bi-gear"] mempty
                            div_
                                [ id_ "view-control"
                                , hxTrigger_
                                    "load\
                                    \, change from:#select-first-week-day\
                                    \, change from:#select-customer\
                                    \, change from:#toggle-slot\
                                    \, change from:#select-window\
                                    \, change from:#toggle-spent\
                                    \"
                                , hxInclude_ "#view-control , #deposits"
                                , hxPost_ $ linkText depositsHistoryLink
                                , hxTarget_ "#deposits"
                                ]
                                $ do
                                    div_ [class_ "d-flex justify-content-end"]
                                        toggle
                                    div_ [class_ "mt-1"]
                                        depositsViewControls
                box "Deposits by Time" configure $ do
                    div_ [class_ "row gx-0"]
                        $ div_
                            [ class_ "col"
                            , id_ "deposits"
                            ]
                            mempty
