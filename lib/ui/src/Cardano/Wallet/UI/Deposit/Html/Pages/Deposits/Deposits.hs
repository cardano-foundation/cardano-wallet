{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.Deposits
    ( depositsH
    , depositsElementH
    , depositH
    , scrollableDeposits
    )
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletPublicIdentity (..)
    )
import Cardano.Wallet.Deposit.Pure
    ( ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Read
    ( Slot
    , WithOrigin (..)
    )
import Cardano.Wallet.Read
    ( SlotNo (..)
    )
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxInclude_
    , hxPost_
    , hxSwap_
    , hxTarget_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( AlertH
    , linkText
    , tdEnd
    , thEnd
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
    , DownTime
    , Expand (..)
    , Window (..)
    , depositsHistoryLink
    , depositsHistoryPageLink
    , depositsHistoryWindowLink
    , depositsLink
    )
import Cardano.Wallet.UI.Deposit.Handlers.Pagination
    ( PaginationHandlers (..)
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
import Cardano.Wallet.UI.Type
    ( WHtml
    )
import Control.Monad
    ( forM_
    , when
    )
import Data.Hashable
    ( Hashable (..)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Ord
    ( Down (..)
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time
    ( DayOfWeek (..)
    )
import Lucid
    ( Attribute
    , Html
    , ToHtml (..)
    , button_
    , checked_
    , class_
    , colspan_
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
    , thead_
    , tr_
    , type_
    , value_
    )
import Servant
    ( ToHttpApiData (toUrlPiece)
    )

import qualified Cardano.Wallet.UI.Common.Html.Scrolling as Scrolling
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

depositsH :: WHtml ()
depositsH = do
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
            simpleField "Fake Data"
                $ div_
                    [ class_ "d-flex justify-content-end align-items-center form-check"
                    ]
                $ input_
                    [ class_ "form-check-input"
                    , type_ "checkbox"
                    , id_ "toggle-fake-data"
                    , name_ "fake-data"
                    , value_ ""
                    , checked_
                    ]

depositsElementH
    :: AlertH
    -> WalletPresent
    -> Html ()
depositsElementH = onWalletPresentH $ \case
    WalletPublicIdentity _xpub _customers ->
        div_
            [ class_ "row mt-3 g-0"
            ]
            $ do
                let configure = do
                        div_ [class_ "d-flex justify-content-end"] $ do
                            let toggle =
                                    button_
                                        [ class_ "btn"
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
                                    \, change from:#toggle-fake-data"
                                , hxInclude_ "#view-control , #deposits"
                                , hxPost_ $ linkText depositsHistoryLink
                                , hxTarget_ "#deposits"
                                ]
                                $ do
                                    box mempty toggle depositsViewControls
                box "Deposits" mempty $ do
                    configure
                    div_ [class_ "row g-0"]
                        $ div_
                            [ class_ "col"
                            , id_ "deposits"
                            ]
                            mempty

scrollableDeposits
    :: Monad m
    => DepositsParams
    -> PaginationHandlers m DownTime (Map DownTime (Maybe Slot, ValueTransfer))
    -> Scrolling.Configuration m DownTime
scrollableDeposits
    params@DepositsParams{depositsSpent, depositsSlot, depositsFakeData}
    (PaginationHandlers previous next retrieve start) =
        Scrolling.Configuration{..}
      where
        scrollableWidget :: [Attribute] -> Html () -> Html ()
        scrollableWidget attrs content =
            fakeOverlay $ do
                let attrs' =
                        [ class_
                            $ "border-top table table-striped table-hover m-0"
                                <> if depositsFakeData then " fake" else ""
                        ]
                table_ (attrs' <> attrs)
                    $ do
                        thead_ [class_ "bg-primary"]
                            $ tr_
                                [ scope_ "row"
                                , class_ "sticky-top my-1"
                                , style_ "z-index: 1"
                                ]
                            $ do
                                thEnd (Just 7) "Time"
                                when depositsSlot
                                    $ thEnd (Just 5) "Slot"
                                thEnd (Just 7) "Received"
                                when depositsSpent
                                    $ thEnd (Just 7) "Spent"
                        content
          where
            fakeOverlay = if depositsFakeData then overlayFakeDataH else id
        scrollableContainer = table_
        retrieveContent time attrs = do
            ds <- retrieve time
            pure
                $ tbody_ attrs
                $ mapM_ (\window -> depositH params Nothing window mempty)
                $ Map.assocs
                    ds
        uniqueScrollingId = "deposits"
        presentFieldName = "page-present"
        controlSelector = "#view-control"
        renderIndex (Down t) = toUrlPiece t
        updateURL (Down t) = linkText . depositsHistoryPageLink . Just $ t
        renderIdOfIndex (Down t) = toUrlPiece $ abs $ hash t

depositH
    :: DepositsParams
    -> Maybe Expand
    -> (DownTime, (Maybe Slot, ValueTransfer))
    -> Html ()
    -> Html ()
depositH
    DepositsParams{depositsSlot, depositsSpent, depositsCustomers}
    mexpand
    (Down time, (slot, ValueTransfer received spent))
    widget
        | expanded = do
            let link =
                    linkText
                        $ depositsHistoryWindowLink
                            (Just time)
                            (Just Collapse)
                trId = T.pack $ "window-" <> show (hash time)
            tr_
                [ id_ trId
                ]
                $ do
                    let depositColumn = if depositsSpent then succ else id
                        slotColumn = if depositsSlot then succ else id
                        columns =
                            T.pack
                                $ show
                                $ depositColumn
                                $ slotColumn (2 :: Int)
                        -- this is bullshit :shrug: . This number refers to the number of columns
                        -- in the container table. Probably one day it will break up.
                        bar = toHtml
                            $ T.pack
                            $ case time of
                                At t -> show t
                                Origin -> "Origin"
                        close =
                            button_
                                [ class_ "btn p-1"
                                , type_ "button"
                                , hxTarget_ $ "#" <> trId
                                , hxSwap_ "outerHTML"
                                , hxPost_ link
                                , hxInclude_ "#view-control"
                                ]
                                $ i_ [class_ "bi bi-x"] mempty
                    td_ [colspan_ columns, class_ "p-0"] $ box bar close $ do
                        widget
                        input_
                            [ type_ "hidden"
                            , name_ "customers"
                            , value_ $ toUrlPiece time
                            ]
        | otherwise = do
            let link =
                    linkText
                        $ depositsHistoryWindowLink
                            (Just time)
                            (Just Expand)
            tr_
                [ scope_ "row"
                , hxTrigger_ "click"
                , hxTarget_ "this"
                , hxSwap_ "outerHTML"
                , hxPost_ link
                , hxInclude_ "#view-control"
                ]
                $ do
                    tdEnd $ do
                        toHtml $ case time of
                            Origin -> "Origin"
                            At t -> show t
                    when depositsSlot
                        $ tdEnd
                        $ toHtml
                        $ case slot of
                            Just Origin -> "Origin"
                            Just (At (SlotNo t)) -> show t
                            Nothing -> "unresolved"
                    tdEnd $ valueH received
                    when depositsSpent
                        $ tdEnd
                        $ valueH spent
      where
        expanded = maybe (time `elem` depositsCustomers) (== Expand) mexpand
