{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Addresses.Transactions
where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( ValueTransfer
    , received
    , spent
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    , tdEnd
    , thEnd
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( Striped (..)
    , Width (..)
    , box
    , record
    , simpleField
    )
import Cardano.Wallet.UI.Deposit.API
    ( customerHistoryLink
    )
import Control.Monad
    ( forM_
    , when
    )
import Lucid
    ( Html
    , HtmlT
    , ToHtml (..)
    , button_
    , checked_
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
    , thead_
    , tr_
    , type_
    , value_
    )

import Cardano.Wallet.Deposit.Read
    ( Slot
    , TxId
    , WithOrigin
    )
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxInclude_
    , hxPost_
    , hxTarget_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Deposit.API.Addresses.Transactions
    ( TransactionHistoryParams (..)
    )
import Cardano.Wallet.UI.Deposit.Html.Common
    ( slotH
    , timeH
    , txIdH
    , valueH
    , withOriginH
    )
import Data.Text
    ( Text
    )
import Data.Time
    ( UTCTime (..)
    , pattern YearMonthDay
    )

import qualified Cardano.Wallet.Read as Read
import qualified Data.Text.Class as T

chainPointToSlotH
    :: Read.ChainPoint
    -> Html ()
chainPointToSlotH cp = case cp of
    Read.GenesisPoint -> toHtml ("Genesis" :: Text)
    Read.BlockPoint (Read.SlotNo n) _ -> toHtml $ show n

txSummaryH
    :: TransactionHistoryParams
    -> (WithOrigin UTCTime, (Slot, TxId, ValueTransfer))
    -> Html ()
txSummaryH
    TransactionHistoryParams{..}
    (time, (slot, txId, value)) = do
        tr_ [scope_ "row"] $ do
            when txHistorySlot
                $ tdEnd
                $ slotH slot
            when txHistoryUTC
                $ tdEnd
                $ withOriginH timeH time
            when txHistoryReceived
                $ tdEnd
                $ valueH
                $ received value
            when txHistorySpent
                $ tdEnd
                $ valueH
                $ spent value
            tdEnd $ txIdH txId

customerHistoryH
    :: Monad m
    => TransactionHistoryParams
    -> [(WithOrigin UTCTime, (Slot, TxId, ValueTransfer))]
    -> HtmlT m ()
customerHistoryH params@TransactionHistoryParams{..} txs =
    table_
        [ class_ "table table-sm table-borderless table-striped table-hover m-0"
        ]
        $ do
            thead_
                $ tr_
                    [ scope_ "row"
                    , class_ "sticky-top my-1"
                    , style_ "z-index: 1"
                    ]
                $ do
                    when txHistorySlot
                        $ thEnd (Just 7) "Slot"
                    when txHistoryUTC
                        $ thEnd (Just 10) "Time"
                    when txHistoryReceived
                        $ thEnd (Just 7) "Deposit"
                    when txHistorySpent
                        $ thEnd (Just 7) "Withdrawal"
                    thEnd Nothing "Id"
            tbody_
                $ mapM_ (toHtml . txSummaryH params) txs

yearOf :: UTCTime -> Integer
yearOf UTCTime{utctDay = YearMonthDay year _ _} = year

monthOf :: UTCTime -> Int
monthOf UTCTime{utctDay = YearMonthDay _ month _} = month

monthsH :: UTCTime -> Html ()
monthsH now = do
    select_
        [ class_ "form-select w-auto m-1 p-1"
        , id_ "select-month"
        , name_ "start-month"
        , style_ "background-image: none"
        ]
        $ forM_ [1 .. 12]
        $ \month -> do
            let select =
                    if month == monthOf now
                        then ([selected_ ""] <>)
                        else id
            option_ (select [value_ $ T.toText month])
                $ toHtml
                $ T.toText month

yearsH :: UTCTime -> UTCTime -> Html ()
yearsH now origin = do
    let firstYear = yearOf origin
        lastYear = yearOf now
    select_
        [ class_ "form-select w-auto m-1 p-1"
        , id_ "select-year"
        , name_ "start-year"
        , style_ "background-image: none"
        ]
        $ forM_ [firstYear .. lastYear]
        $ \year -> do
            let select =
                    if year == lastYear
                        then ([selected_ ""] <>)
                        else id
            option_ (select [value_ $ T.toText year])
                $ toHtml
                $ T.toText year

transactionsViewControls :: UTCTime -> UTCTime -> Html ()
transactionsViewControls now origin =
    div_ [class_ "collapse", id_ "columns-control"] $ do
        record Nothing Auto NotStriped $ do
            simpleField "UTC"
                $ div_
                    [ class_ "d-flex justify-content-end align-items-center form-check"
                    ]
                $ input_
                    [ class_ "form-check-input"
                    , type_ "checkbox"
                    , id_ "toggle-utc"
                    , hxTrigger_ "change"
                    , name_ "utc"
                    , value_ ""
                    , checked_
                    ]
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
            simpleField "Deposit"
                $ div_
                    [ class_ "d-flex justify-content-end align-items-center form-check"
                    ]
                $ input_
                    [ class_ "form-check-input"
                    , type_ "checkbox"
                    , id_ "toggle-deposit"
                    , name_ "received"
                    , value_ ""
                    , checked_
                    ]
            simpleField "Withdrawal"
                $ div_
                    [ class_ "d-flex justify-content-end align-items-center form-check"
                    ]
                $ input_
                    [ class_ "form-check-input"
                    , type_ "checkbox"
                    , id_ "toggle-withdrawal"
                    , name_ "spent"
                    , value_ ""
                    ]
            simpleField "Sorting"
                $ div_
                    [ class_ "d-flex justify-content-end align-items-center"
                    ]
                $ select_
                    [ class_ "form-select w-auto m-1 p-1"
                    , id_ "select-sorting"
                    , name_ "sorting"
                    , style_ "background-image: none"
                    ]
                $ do
                    option_ [selected_ "", value_ "desc"] "Descending"
                    option_ [value_ "asc"] "Ascending"
            simpleField "From"
                $ div_
                    [ class_ "d-flex justify-content-end align-items-center"
                    ]
                $ do
                    yearsH now origin
                    monthsH now

transactionsElementH :: UTCTime -> UTCTime -> Html ()
transactionsElementH now origin = do
    div_
        [ class_ "row mt-2 gx-0"
        , hxTrigger_
            "load\
            \, change from:#toggle-utc\
            \, change from:#select-customer\
            \, change from:#toggle-slot\
            \, change from:#toggle-deposit\
            \, change from:#toggle-withdrawal\
            \, change from:#select-sorting\
            \, change from:#select-month\
            \, change from:#select-year"
        , hxInclude_ "#view-control"
        , hxPost_ $ linkText customerHistoryLink
        , hxTarget_ "#transactions"
        ]
        $ do
            let configure =
                    div_ [class_ "d-flex justify-content-end p-0"] $ do
                        let toggle = button_
                                [ class_ "btn p-0"
                                , type_ "button"
                                , data_ "bs-toggle" "collapse"
                                , data_ "bs-target" "#columns-control"
                                ]
                                $ div_
                                $ do
                                    i_ [class_ "bi bi-gear"] mempty
                        div_ $ do
                            div_
                                [class_ "d-flex justify-content-end"]
                                toggle
                            div_ [class_ "mt-1"]
                                $ transactionsViewControls now origin
            box "Transactions" configure $ do
                div_ [class_ "row gx-0"] $ do
                    div_
                        [ class_ "col"
                        , id_ "transactions"
                        ]
                        mempty
