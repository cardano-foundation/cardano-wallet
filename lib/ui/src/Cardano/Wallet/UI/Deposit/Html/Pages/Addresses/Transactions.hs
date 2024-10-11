{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Addresses.Transactions
where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( TxSummary (..)
    , received
    , spent
    )
import Cardano.Wallet.Read.Hash
    ( hashToStringAsHex
    )
import Cardano.Wallet.Read.Tx
    ( hashFromTxId
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    , tdEnd
    , thEnd
    , truncatableText
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( Striped (..)
    , Width (..)
    , box
    , record
    , simpleField
    )
import Cardano.Wallet.UI.Deposit.API
    ( TransactionHistoryParams (..)
    , customerHistoryLink
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
    , span_
    , style_
    , table_
    , tbody_
    , td_
    , thead_
    , tr_
    , type_
    , value_
    )

import Cardano.Wallet.Deposit.Read
    ( Slot
    , WithOrigin (..)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Text
    ( Text
    )

import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxInclude_
    , hxPost_
    , hxTarget_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Deposit.Html.Lib
    ( overlayFakeDataH
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time
    ( UTCTime (..)
    , defaultTimeLocale
    , formatTime
    , pattern YearMonthDay
    )
import Numeric
    ( showFFloatAlt
    )

import qualified Cardano.Wallet.Read as Read
import qualified Data.Map.Strict as Map
import qualified Data.Text.Class as T

chainPointToUTCH
    :: Map Slot (WithOrigin UTCTime)
    -> Read.ChainPoint
    -> Html ()
chainPointToUTCH
    times
    cp = case Map.lookup (Read.slotFromChainPoint cp) times of
        Just (At t) ->
            toHtml
                $ formatTime defaultTimeLocale "%F %T" t
        Just Origin -> toHtml ("Genesis" :: Text)
        Nothing -> toHtml ("Unknown" :: Text)

chainPointToSlotH
    :: Read.ChainPoint
    -> Html ()
chainPointToSlotH cp = case cp of
    Read.GenesisPoint -> toHtml ("Genesis" :: Text)
    Read.BlockPoint (Read.SlotNo n) _ -> toHtml $ show n

valueH :: Read.Value -> Html ()
valueH (Read.ValueC (Read.CoinC c) _) = do
    span_ $ toHtml $ a ""
    span_ [class_ "opacity-25"] "₳"
  where
    a = showFFloatAlt @Double (Just 2) $ fromIntegral c / 1_000_000

txSummaryH
    :: TransactionHistoryParams
    -> Map Slot (WithOrigin UTCTime)
    -> (Int, TxSummary)
    -> Html ()
txSummaryH
    TransactionHistoryParams{..}
    times
    (index, TxSummaryC{txSummarized, txChainPoint, txTransfer}) = do
        tr_ [scope_ "row"] $ do
            when txHistorySlot
                $ tdEnd
                $ chainPointToSlotH txChainPoint
            when txHistoryUTC
                $ tdEnd
                $ chainPointToUTCH times txChainPoint
            when txHistoryReceived
                $ tdEnd
                $ valueH
                $ received txTransfer
            when txHistorySpent
                $ tdEnd
                $ valueH
                $ spent txTransfer
            td_ [scope_ "col", class_ "flex-fill align-bottom"]
                $ truncatableText ("tx-id-text-" <> toText index)
                $ toHtml
                $ hashToStringAsHex
                $ hashFromTxId txSummarized

customerHistoryH
    :: Monad m
    => Bool
    -> TransactionHistoryParams
    -> Map Slot (WithOrigin UTCTime)
    -> [TxSummary]
    -> HtmlT m ()
customerHistoryH fake params@TransactionHistoryParams{..} times txs =
    fakeOverlay $ do
        table_
            [ class_
                $ "border-top table table-striped table-hover m-0"
                    <> if fake then " fake" else ""
            ]
            $ do
                thead_ $ tr_ [scope_ "row"] $ do
                    when txHistorySlot
                        $ thEnd (Just 7) "Slot"
                    when txHistoryUTC
                        $ thEnd (Just 7) "Time"
                    when txHistoryReceived
                        $ thEnd (Just 7) "Received"
                    when txHistorySpent
                        $ thEnd (Just 7) "Spent"
                    thEnd Nothing "Tx Id"
                tbody_
                    $ mapM_ (toHtml . txSummaryH params times)
                    $ zip [0 ..] txs
  where
    fakeOverlay = if fake then overlayFakeDataH else id

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
    div_ [class_ "d-flex justify-content-end"] $ do
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
                simpleField "Received"
                    $ div_
                        [ class_ "d-flex justify-content-end align-items-center form-check"
                        ]
                    $ input_
                        [ class_ "form-check-input"
                        , type_ "checkbox"
                        , id_ "toggle-received"
                        , name_ "received"
                        , value_ ""
                        , checked_
                        ]
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
        [ class_ "row mt-2 g-0"
        , hxTrigger_
            "load\
            \, change from:#toggle-utc\
            \, change from:#select-customer\
            \, change from:#toggle-slot\
            \, change from:#toggle-received\
            \, change from:#toggle-spent\
            \, change from:#select-sorting\
            \, change from:#select-month\
            \, change from:#select-year"
        , hxInclude_ "#view-control"
        , hxPost_ $ linkText customerHistoryLink
        , hxTarget_ "#transactions"
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
            box "Transactions" configure $ do
                transactionsViewControls now origin
                div_ [class_ "row g-0"] $ do
                    div_
                        [ class_ "col"
                        , id_ "transactions"
                        ]
                        mempty
