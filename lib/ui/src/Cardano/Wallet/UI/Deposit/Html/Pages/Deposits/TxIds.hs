{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.TxIds
    ( scrollableDepositsCustomersTxIds
    )
where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( Customer
    , ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Read
    ( TxId
    )
import Cardano.Wallet.Read
    ( hashFromTxId
    )
import Cardano.Wallet.Read.Hash
    ( hashToStringAsHex
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( WithCopy (..)
    , linkText
    , tdEnd
    , thEnd
    , truncatableText
    )
import Cardano.Wallet.UI.Deposit.API
    ( DepositsParams (..)
    , DownTime
    , depositsHistoryCustomersTxIdsPageLink
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
import Control.Lens
    ( _1
    , view
    )
import Control.Monad
    ( when
    )
import Data.List
    ( sortOn
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
import Lucid
    ( Attribute
    , Html
    , ToHtml (..)
    , class_
    , scope_
    , style_
    , table_
    , tbody_
    , thead_
    , tr_
    )
import Servant
    ( ToHttpApiData (toUrlPiece)
    )

import qualified Cardano.Wallet.UI.Common.Html.Scrolling as Scrolling
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

scrollableDepositsCustomersTxIds
    :: Monad m
    => DepositsParams
    -> DownTime
    -> Customer
    -> PaginationHandlers m TxId (Map TxId ValueTransfer)
    -> Scrolling.Configuration m TxId
scrollableDepositsCustomersTxIds
    params@DepositsParams{depositsSpent, depositsFakeData}
    (Down time)
    customer
    PaginationHandlers{previousPageIndex, nextPageIndex, retrievePage, startingIndex} =
        Scrolling.Configuration{..}
      where
        previous = previousPageIndex
        next = nextPageIndex
        start = startingIndex

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
                                , style_ "z-index: 2"
                                ]
                            $ do
                                thEnd Nothing "Transaction"
                                thEnd (Just 7) "Deposit"
                                when depositsSpent
                                    $ thEnd (Just 7) "Spent"
                        content
          where
            fakeOverlay = if depositsFakeData then overlayFakeDataH else id
        scrollableContainer = table_
        retrieveContent txId attrs = do
            xs <- retrievePage txId
            pure
                $ tbody_ attrs
                $ mapM_ (depositByTxIdH params)
                $ sortOn (view _1)
                $ Map.assocs xs
        uniqueScrollingId = "deposit-customers-tx-ids"
        presentFieldName = "tx-id-page-present"
        controlSelector = "#view-control"
        renderIndex = toUrlPiece
        updateURL txId =
            linkText
                $ depositsHistoryCustomersTxIdsPageLink
                    (Just time)
                    (Just customer)
                    (Just txId)
        renderIdOfIndex = T.replace " " "-" . toUrlPiece

depositByTxIdH
    :: DepositsParams
    -> (TxId, ValueTransfer)
    -> Html ()
depositByTxIdH
    DepositsParams{depositsSpent}
    (txId, ValueTransfer received spent) = do
        tr_ [scope_ "row"] $ do
            tdEnd $ txIdH txId
            tdEnd $ valueH received
            when depositsSpent
                $ tdEnd
                $ valueH spent

txIdH :: TxId -> Html ()
txIdH txId =
    truncatableText WithCopy ("tx-id-text-" <> toText (take 16 h))
        $ toHtml h
  where
    h =
        hashToStringAsHex
            $ hashFromTxId
                txId
