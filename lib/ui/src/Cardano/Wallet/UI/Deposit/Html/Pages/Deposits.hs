{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Deposits
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletPublicIdentity (..)
    )
import Cardano.Wallet.Deposit.Pure
    ( ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , WithOrigin (..)
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
    , truncatableText
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( Striped (..)
    , Width (..)
    , box
    , record
    , simpleField
    , sseH
    )
import Cardano.Wallet.UI.Deposit.API
    ( DepositsParams (..)
    , Window (..)
    , depositsHistoryLink
    , depositsLink
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits
    ( DepositsHistory
    , DepositsWindow (..)
    )
import Cardano.Wallet.UI.Deposit.Html.Lib
    ( overlayFakeDataH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Addresses.Transactions
    ( valueH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( WalletPresent (..)
    , onWalletPresentH
    )
import Cardano.Wallet.UI.Lib.Address
    ( encodeMainnetAddress
    )
import Cardano.Wallet.UI.Type
    ( WHtml
    )
import Control.Monad
    ( forM_
    , when
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Ord
    ( Down (..)
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time
    ( DayOfWeek (..)
    , UTCTime
    )
import Lucid
    ( Html
    , HtmlT
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
    , scope_
    , select_
    , selected_
    , style_
    , table_
    , tbody_
    , td_
    , th_
    , thead_
    , tr_
    , type_
    , value_
    )
import Servant
    ( ToHttpApiData (toUrlPiece)
    )

import qualified Data.Map.Monoidal.Strict as MonoidalMap

depositsH :: WHtml ()
depositsH = do
    sseH depositsLink "addresses" ["wallet"]

customerAddressH :: Monad m => Address -> HtmlT m ()
customerAddressH addr = truncatableText "address-text" $ toHtml encodedAddr
  where
    encodedAddr = encodeMainnetAddress addr

depositsViewControls :: Html ()
depositsViewControls =
    div_ [class_ "d-flex justify-content-end"] $ do
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
                        ]
                    $ forM_ [Minute5 .. Year]
                    $ \window -> do
                        let selected = case window of
                                Day -> (selected_ "" :)
                                _ -> id
                        option_ (selected [value_ $ toUrlPiece window])
                            $ toHtml
                            $ toText window

depositsElementH
    :: AlertH
    -> WalletPresent
    -> Html ()
depositsElementH = onWalletPresentH $ \case
    WalletPublicIdentity _xpub _customers ->
        div_ [id_ "view-control"] $ do
            div_
                [ class_ "row mt-3 g-0"
                , hxTrigger_
                    "load\
                    \, change from:#first-week-day\
                    \, change from:#select-customer\
                    \, change from:#toggle-slot\
                    \, change from:#select-window"
                , hxInclude_ "#view-control"
                , hxPost_ $ linkText depositsHistoryLink
                , hxTarget_ "#deposits"
                ]
                $ do
                    let configure =
                            div_ [class_ "d-flex justify-content-end"] $ do
                                button_
                                    [ class_ "btn"
                                    , type_ "button"
                                    , data_ "bs-toggle" "collapse"
                                    , data_ "bs-target" "#columns-control"
                                    ]
                                    $ div_
                                    $ do
                                        i_ [class_ "bi bi-gear"] mempty
                    box "Deposits" configure $ do
                        depositsViewControls
                        div_ [class_ "row g-0"]
                            $ div_
                                [ class_ "col"
                                , id_ "deposits"
                                ]
                                mempty

depositsHistoryH :: Bool -> DepositsParams -> DepositsHistory -> Html ()
depositsHistoryH fake params@DepositsParams{..} ds =
    fakeOverlay $ do
        table_
            [ class_
                $ "border-top table table-striped table-hover m-0"
                    <> if fake then " fake" else ""
            ]
            $ do
                thead_ $ tr_ [scope_ "row"] $ do
                    th_
                        [ scope_ "col"
                        , class_ "text-end"
                        , style_ "width: 7em"
                        ]
                        "Time"
                    when depositsSlot
                        $ th_
                            [ scope_ "col"
                            , class_ "text-end"
                            , style_ "width: 7em"
                            ]
                            "Slot"
                    th_
                        [ scope_ "col"
                        , class_ "text-end"
                        , style_ "width: 7em"
                        ]
                        "Received"
                    th_
                        [ scope_ "col"
                        , class_ "text-end"
                        , style_ "width: 7em"
                        ]
                        "Spent"
                tbody_
                    $ mapM_ (depositH params)
                    $ MonoidalMap.assocs ds
  where
    fakeOverlay = if fake then overlayFakeDataH else id

depositH
    :: DepositsParams
    -> (Down (WithOrigin UTCTime), DepositsWindow)
    -> Html ()
depositH DepositsParams{..} (Down time, DepositsWindow{..}) = do
    tr_ [scope_ "row"] $ do
        td_ [class_ "text-end"] $ toHtml $ case time of
            Origin -> "Origin"
            At t -> show t
        when depositsSlot
            $ td_ [class_ "text-end"]
            $ toHtml
            $ show depositsSlot
        let ValueTransfer received spent = fold depositsWindowTransfers
        td_ [class_ "text-end"] $ valueH received
        td_ [class_ "text-end"] $ valueH spent
