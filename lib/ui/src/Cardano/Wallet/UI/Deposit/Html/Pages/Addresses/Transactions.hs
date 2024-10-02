{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
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
    , truncatableText
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( record
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
    , ToHtml (..)
    , button_
    , checked_
    , class_
    , data_
    , div_
    , id_
    , input_
    , name_
    , scope_
    , span_
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
    ( UTCTime
    , defaultTimeLocale
    , formatTime
    )
import Numeric
    ( showFFloatAlt
    )

import qualified Cardano.Wallet.Read as Read
import qualified Data.Map.Strict as Map

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
                $ td_ [scope_ "col", class_ "text-end"]
                $ chainPointToSlotH txChainPoint
            when txHistoryUTC
                $ td_ [scope_ "col", class_ "text-end"]
                $ chainPointToUTCH times txChainPoint
            when txHistoryReceived
                $ td_ [scope_ "col", class_ "text-end"]
                $ valueH
                $ received txTransfer
            when txHistorySpent
                $ td_ [scope_ "col", class_ "text-end"]
                $ valueH
                $ spent txTransfer
            td_ [scope_ "col", class_ "flex-fill"]
                $ truncatableText ("tx-id-text-" <> toText index)
                $ toHtml
                $ hashToStringAsHex
                $ hashFromTxId txSummarized

customerHistoryH
    :: Bool
    -> TransactionHistoryParams
    -> [TxSummary]
    -> Map Slot (WithOrigin UTCTime)
    -> Html ()
customerHistoryH fake params@TransactionHistoryParams{..} txs times =
    fakeOverlay $ do
        table_
            [ class_
                $ "table table-striped table-bordered table-hover"
                    <> if fake then " fake" else ""
            ]
            $ do
                thead_ $ tr_ [scope_ "row"] $ do
                    when txHistorySlot
                        $ th_
                            [ scope_ "col"
                            , class_ "text-end"
                            , style_ "width: 7em"
                            ]
                            "Slot"
                    when txHistoryUTC
                        $ th_
                            [ scope_ "col"
                            , class_ "text-end"
                            , style_ "width: 7em"
                            ]
                            "Time"
                    when txHistoryReceived
                        $ th_
                            [ scope_ "col"
                            , class_ "text-end"
                            , style_ "width: 7em"
                            ]
                            "Received"
                    when txHistorySpent
                        $ th_
                            [ scope_ "col"
                            , class_ "text-end"
                            , style_ "width: 7em"
                            ]
                            "Spent"
                    th_
                        [ scope_ "col"
                        , class_ "text-end"
                        , style_ "width: auto"
                        ]
                        "Tx Id"
                tbody_ $ forM_ (zip [0 ..] txs) $ txSummaryH params times
  where
    fakeOverlay = if fake then overlayFakeDataH else id

transactionsViewControls :: Html ()
transactionsViewControls = do
    div_ [class_ "row"] $ do
        div_ [class_ "col"] $ do
            button_
                [ class_ "btn btn-secondary"
                , type_ "button"
                , data_ "bs-toggle" "collapse"
                , data_ "bs-target" "#columns-control"
                ]
                "Configure"
        div_ [class_ "col"] $ do
            div_ [class_ "collapse", id_ "columns-control"] $ do
                record Nothing $ do
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

transactionsElementH :: Html ()
transactionsElementH = do
    div_
        [ hxTrigger_
            "load\
            \, change from:#toggle-utc\
            \, change from:#select-customer\
            \, change from:#toggle-slot\
            \, change from:#toggle-received\
            \, change from:#toggle-spent"
        , hxInclude_ "#view-control"
        , hxPost_ $ linkText customerHistoryLink
        , hxTarget_ "#transactions"
        ]
        $ do
            transactionsViewControls
            div_ [class_ "row"] $ do
                div_
                    [ class_ "col"
                    , id_ "transactions"
                    ]
                    mempty
