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
    , Slot
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
    , Expand (..)
    , Window (..)
    , depositsHistoryExtendLink
    , depositsHistoryLink
    , depositsHistoryWindowLink
    , depositsLink
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
import Data.Monoid
    ( First (..)
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
                        div_ [class_ "d-flex justify-content-end sticky-top", style_ "z:3"] $ do
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

depositsPartsH :: DepositsParams -> SolveAddress -> DepositsHistory -> Html ()
depositsPartsH params solveAddress ds = do
    let ((before, after), lasted) =
            let
                xs = MonoidalMap.assocs ds
                l = length xs
            in
                (splitAt (l `div` 2) xs, snd <$> unsnoc xs)
    tbody_ $ mapM_ (depositH params solveAddress Nothing) before
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
    tbody_ $ mapM_ (depositH params solveAddress Nothing) after
    case lasted of
        Just (Down time, _)
            | time > Origin -> do
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
    params@DepositsParams{depositsSpent, depositsSlot}
    customerOfAddress
    DepositsWindow{depositsWindowTransfers} =
        do
            let xs =
                    MonoidalMap.assocs (fmap fold depositsWindowTransfers)
                        <&> \(addr, (First slot, ValueTransfer received spent)) ->
                            ( customerOfAddress addr
                            , slot
                            , addr
                            , ValueTransfer received spent
                            )
            table_ [class_ "table table-striped table-hover m-0"] $ do
                thead_ $ tr_ [scope_ "row"] $ do
                    thEnd Nothing "Addr"
                    when depositsSlot
                        $ thEnd (Just 6) "Slot"
                    thEnd (Just 6) "Customer"
                    thEnd (Just 7) "Received"
                    when depositsSpent
                        $ thEnd (Just 7) "Spent"
                tbody_ $ mapM_ (depositByAddressH params) $ sortOn (view _1) xs

depositByAddressH
    :: DepositsParams
    -> (Maybe Customer, Maybe Slot, Address, ValueTransfer)
    -> Html ()
depositByAddressH
    DepositsParams{depositsSpent, depositsSlot}
    (mcustomer, mslot, addr, ValueTransfer received spent) = do
        tr_ [scope_ "row"] $ do
            tdEnd $ customerAddressH addr
            when depositsSlot
                $ tdEnd
                $ toHtml
                $ case mslot of
                    Just slot -> case slot of
                        Origin -> "Origin"
                        At (SlotNo t) -> show t
                    Nothing -> "ERROR"
            tdEnd $ case mcustomer of
                Just c -> toHtml $ show c
                Nothing -> "External"
            tdEnd $ valueH received
            when depositsSpent
                $ tdEnd
                $ valueH spent

depositsHistoryH :: DepositsParams -> SolveAddress -> DepositsHistory -> Html ()
depositsHistoryH params@DepositsParams{..} solveAddress ds = fakeOverlay $ do
    table_
        [ class_
            $ "border-top table table-striped table-hover m-0"
                <> if depositsFakeData then " fake" else ""
        ]
        $ do
            thead_ $ tr_ [scope_ "row"] $ do
                thEnd (Just 7) "Time"
                when depositsSlot
                    $ thEnd (Just 5) "Slot"
                thEnd (Just 7) "Received"
                when depositsSpent
                    $ thEnd (Just 7) "Spent"
            tbody_ $ do
                depositsPartsH params solveAddress ds
  where
    fakeOverlay = if depositsFakeData then overlayFakeDataH else id

depositH
    :: DepositsParams
    -> SolveAddress
    -> Maybe Expand
    -> (Down (WithOrigin UTCTime), DepositsWindow)
    -> Html ()
depositH
    params@DepositsParams{depositsSlot, depositsSpent, depositsDetails}
    solveAddress
    mexpand
    (Down time, DepositsWindow{..})
        | expand = do
            let link = linkText $ depositsHistoryWindowLink (Just time) (Just Collapse)
                trId = T.pack $ "window-" <> show (hash time)
            tr_
                [ id_ trId
                ]
                $ do
                    let depositColumn = if depositsSpent then succ else id
                        slotColumn = if depositsSlot then succ else id
                        columns = T.pack $ show $ depositColumn $ slotColumn (2 :: Int)
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
                        depositsHistoryWindowH
                            params
                            solveAddress
                            DepositsWindow{..}
                        input_ [type_ "hidden", name_ "details", value_ $ toUrlPiece time]
        | otherwise = do
            let link = linkText $ depositsHistoryWindowLink (Just time) (Just Expand)
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
                        $ case depositsWindowSlot of
                            Origin -> "Origin"
                            At (SlotNo t) -> show t
                    let (_, ValueTransfer received spent) =
                            fold $ fold depositsWindowTransfers
                    tdEnd $ valueH received
                    when depositsSpent
                        $ tdEnd
                        $ valueH spent
      where
        expand = maybe (time `elem` depositsDetails) (== Expand) mexpand
