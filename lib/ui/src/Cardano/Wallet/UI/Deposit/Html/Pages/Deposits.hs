{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Deposits
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletPublicIdentity (..)
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    , ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , WithOrigin (..)
    )
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxGet_
    , hxInclude_
    , hxPost_
    , hxSwap_
    , hxTarget_
    , hxTrigger_
    , hxVals_
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
    , Window (..)
    , depositsHistoryExtendLink
    , depositsHistoryLink
    , depositsHistoryWindowLink
    , depositsLink
    , emptinessLink
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits
    ( DepositsHistory
    , DepositsWindow (..)
    , SolveAddress
    )
import Cardano.Wallet.UI.Deposit.Html.Lib
    ( overlayFakeDataH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Addresses
    ( customerAddressH
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
import Control.Lens
    ( _1
    , unsnoc
    , view
    , (<&>)
    )
import Control.Monad
    ( forM_
    , when
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Hashable
    ( Hashable (..)
    )
import Data.List
    ( sortOn
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

import qualified Data.Map.Monoidal.Strict as MonoidalMap
import qualified Data.Text as T

depositsH :: WHtml ()
depositsH = do
    sseH depositsLink "addresses" ["wallet"]

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
        div_ [id_ "view-control"] $ do
            div_
                [ class_ "row mt-3 g-0"
                , hxTrigger_
                    "load\
                    \, change from:#select-first-week-day\
                    \, change from:#select-customer\
                    \, change from:#toggle-slot\
                    \, change from:#select-window\
                    \, change from:#toggle-spent\
                    \, change from:#toggle-fake-data"
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

depositsPartsH :: DepositsParams -> DepositsHistory -> Html ()
depositsPartsH params ds = do
    let ((before, after), lasted) =
            let
                xs = MonoidalMap.assocs ds
                l = length xs
            in
                (splitAt (l `div` 2) xs, snd <$> unsnoc xs)
    tbody_ $ mapM_ (depositH params) before
    case lasted of
        Just (Down (At _time), _) -> do
            tbody_
                [ hxTrigger_ "intersect once"
                , hxTarget_ "#load-more"
                , hxSwap_ "outerHTML"
                , hxInclude_ "#view-control"
                , hxPost_ $ linkText depositsHistoryExtendLink
                ]
                mempty
        _ -> mempty
    tbody_ $ mapM_ (depositH params) after
    case lasted of
        Just (Down time, _) -> do
            tbody_
                [ id_ "load-more"
                ]
                $ do
                    tr_ [scope_ "row"] $ td_ "to be loaded"
                    input_
                        [ type_ "hidden"
                        , id_ "revealed"
                        , name_ "view-start"
                        , value_
                            $ toUrlPiece time
                        ]
        _ -> mempty

depositsHistoryWindowH
    :: DepositsParams
    -> SolveAddress
    -> DepositsWindow
    -> Html ()
depositsHistoryWindowH
    params@DepositsParams{depositsViewStart, depositsSpent}
    customerOfAddress
    DepositsWindow{depositsWindowTransfers} = do
        let xs =
                MonoidalMap.assocs (fmap fold depositsWindowTransfers)
                    <&> \(addr, ValueTransfer received spent) ->
                        ( customerOfAddress addr
                        , addr
                        , ValueTransfer received spent
                        )
            swapTarget = maybe "" (T.pack . show . hash) depositsViewStart
        let columns = if depositsSpent then "3" else "2"
            -- this is bullshit :shrug: . This number refers to the number of columns
            -- in the container table. Probably one day it will break up.
        td_ [colspan_ columns] $ table_ [class_ "table table-striped table-hover m-0"] $ do
            div_ [class_ "d-flex justify-content-end"] $ do
                button_
                    [ class_ "btn btn-secondary p-1"
                    , type_ "button"
                    , hxTarget_ $ "#window-" <> swapTarget
                    , hxGet_ $ linkText emptinessLink
                    ]
                    $ i_ [class_ "bi bi-x"] mempty
            thead_ $ tr_ [scope_ "row"] $ do
                thEnd Nothing "Addr"
                thEnd (Just 6) "Customer"
                thEnd (Just 7) "Received"
                when depositsSpent
                    $ thEnd (Just 7) "Spent"
            tbody_ $ mapM_ (depositByAddressH params) $ sortOn (view _1) xs

depositByAddressH
    :: DepositsParams
    -> (Maybe Customer, Address, ValueTransfer)
    -> Html ()
depositByAddressH DepositsParams{depositsSpent}
    (mcustomer, addr, ValueTransfer received spent) = do
    tr_ [scope_ "row"] $ do
        tdEnd $ customerAddressH addr
        tdEnd $ case mcustomer of
            Just c -> toHtml $ show c
            Nothing -> "External"
        tdEnd $ valueH received
        when depositsSpent
            $ tdEnd $ valueH spent

depositsHistoryH :: DepositsParams -> DepositsHistory -> Html ()
depositsHistoryH params@DepositsParams{..} ds = fakeOverlay $ do
    table_
        [ class_
            $ "border-top table table-striped table-hover m-0"
                <> if depositsFakeData then " fake" else ""
        ]
        $ do
            thead_ $ tr_ [scope_ "row"] $ do
                thEnd (Just 7) "Time"
                when depositsSlot
                    $ thEnd (Just 7) "Slot"
                thEnd (Just 7) "Received"
                when depositsSpent
                    $ thEnd (Just 7) "Spent"
            tbody_ $ do
                depositsPartsH params ds
  where
    fakeOverlay = if depositsFakeData then overlayFakeDataH else id

depositH
    :: DepositsParams
    -> (Down (WithOrigin UTCTime), DepositsWindow)
    -> Html ()
depositH DepositsParams{depositsSlot, depositsSpent}
    (Down time, DepositsWindow{..}) = do
    tr_
        [ scope_ "row"
        , hxTrigger_ "click"
        , hxTarget_ "next"
        , hxPost_ $ linkText depositsHistoryWindowLink
        , hxVals_ $ "{\"view-start\":\"" <> toUrlPiece time <> "\"}"
        ]
        $ do
            tdEnd $ toHtml $ case time of
                Origin -> "Origin"
                At t -> show t
            when depositsSlot
                $ tdEnd
                $ toHtml
                $ show depositsSlot
            let ValueTransfer received spent =
                    fold $ fold depositsWindowTransfers
            tdEnd $ valueH received
            when depositsSpent
                $ tdEnd $ valueH spent
    tr_ [id_ $ "window-" <> T.pack (show $ hash time)] mempty
