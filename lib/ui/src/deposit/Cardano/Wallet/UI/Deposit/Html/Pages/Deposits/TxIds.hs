{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldMap" #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.TxIds
    ( scrollableDepositsCustomersTxIds
    )
where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( Customer
    , ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( DownTime
    )
import Cardano.Wallet.Deposit.Read
    ( TxId
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    , tdEnd
    , thEnd
    )
import Cardano.Wallet.UI.Deposit.API
    ( depositsTxIdsPaginatingLink
    )
import Cardano.Wallet.UI.Deposit.API.Deposits.Deposits
    ( DepositsParams (..)
    )
import Cardano.Wallet.UI.Deposit.Html.Common
    ( txIdH
    , valueH
    )
import Cardano.Wallet.UI.Lib.Pagination.Type
    ( Paginate (..)
    , PaginateM
    )
import Control.Lens
    ( _1
    , view
    , (<&>)
    )
import Control.Monad
    ( when
    )
import Data.Foldable
    ( fold
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
import Lucid
    ( Attribute
    , Html
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
    -> PaginateM m TxId (Map TxId ValueTransfer)
    -> Scrolling.Configuration m TxId
scrollableDepositsCustomersTxIds
    params@DepositsParams{depositsSpent}
    (Down time)
    customer
    Paginate
        { previousIndex
        , nextIndex
        , pageAtIndex
        , minIndex
        } =
        Scrolling.Configuration{..}
      where
        scrollableWidget :: [Attribute] -> Html () -> Html ()
        scrollableWidget attrs content = do
            let attrs' =
                    [ class_ "table-sm table-borderless table table-striped table-hover m-0"
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
        scrollableContainer = table_
        retrieveContent txId attrs = do
            mxs <- pageAtIndex txId
            pure $ fold $ mxs <&> \(_, xs) ->
                tbody_ attrs
                    $ mapM_ (depositByTxIdH params)
                    $ sortOn (view _1)
                    $ Map.assocs xs
        uniqueScrollingId = "deposit-customers-tx-ids"
        presentFieldName = "tx-ids-paginating-presence"
        controlSelector = "#view-control"
        renderIndex = toUrlPiece
        updateURL txId =
            linkText
                $ depositsTxIdsPaginatingLink
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
    (txId, ValueTransferC{received, spent}) = do
        tr_ [scope_ "row"] $ do
            tdEnd $ txIdH txId
            tdEnd $ valueH received
            when depositsSpent
                $ tdEnd
                $ valueH spent
