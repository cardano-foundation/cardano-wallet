{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.Customers
    ( scrollableDepositsCustomers
    , depositByCustomerH
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
    ( Address
    , WithOrigin
    )
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxInclude_
    , hxPost_
    , hxSwap_
    , hxTarget_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    , tdEnd
    , thEnd
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( box
    )
import Cardano.Wallet.UI.Deposit.API.Common
    ( Expand (..)
    )
import Cardano.Wallet.UI.Deposit.API.Deposits.Deposits
    ( DepositsParams (..)
    )
import Cardano.Wallet.UI.Deposit.Html.Common
    ( valueH
    )
import Cardano.Wallet.UI.Lib.Pagination.Type
    ( Paginate (..)
    , PaginateM
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
import Data.Time
    ( UTCTime
    )
import Lucid
    ( Attribute
    , Html
    , ToHtml (..)
    )
import Lucid.Html5
    ( button_
    , class_
    , colspan_
    , div_
    , i_
    , id_
    , input_
    , name_
    , scope_
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
    ( Link
    , ToHttpApiData (toUrlPiece)
    )

import qualified Cardano.Wallet.UI.Common.Html.Scrolling as Scrolling
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

scrollableDepositsCustomers
    :: Monad m
    => DepositsParams
    -> (Maybe (WithOrigin UTCTime) -> Maybe Customer -> Link)
    -> (Maybe (WithOrigin UTCTime) -> Maybe Customer -> Maybe Expand -> Link)
    -> DownTime
    -> PaginateM m Customer (Map Customer (Maybe Address, ValueTransfer))
    -> Scrolling.Configuration m Customer
scrollableDepositsCustomers
    params@DepositsParams{depositsSpent}
    depositsCustomersPaginatingLink
    depositsTxIdsLink
    (Down time)
    Paginate{previousIndex, nextIndex, pageAtIndex, minIndex} =
        Scrolling.Configuration{..}
      where
        scrollableWidget :: [Attribute] -> Html () -> Html ()
        scrollableWidget attrs content = do
            let attrs' =
                    [ class_ "table table-sm table-borderless table-striped table-hover m-0"
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
                            thEnd (Just 6) "Customer"
                            thEnd (Just 7) "Deposit"
                            when depositsSpent
                                $ thEnd (Just 7) "Spent"
                    content
        scrollableContainer = table_
        retrieveContent customer attrs = do
            mxs <- pageAtIndex customer
            case mxs of
                Nothing -> pure mempty
                Just (_, xs) ->
                    pure
                        $ tbody_ attrs
                        $ mapM_
                            ( \transfers ->
                                depositByCustomerH
                                    params
                                    depositsTxIdsLink
                                    Nothing
                                    (Down time)
                                    transfers
                                    mempty
                            )
                        $ sortOn (view _1)
                        $ Map.assocs xs
        uniqueScrollingId = "deposit-customers"
        presentFieldName = "customers-paginating-presence"
        controlSelector = "#view-control"
        renderIndex = toUrlPiece
        updateURL c =
            linkText
                $ depositsCustomersPaginatingLink
                    (Just time)
                    (Just c)
        renderIdOfIndex = T.replace " " "-" . toUrlPiece

depositByCustomerH
    :: DepositsParams
    -> (Maybe (WithOrigin UTCTime) -> Maybe Customer -> Maybe Expand -> Link)
    -> Maybe Expand
    -> DownTime
    -> (Customer, (Maybe Address, ValueTransfer))
    -> ([Attribute] -> Html ())
    -> Html ()
depositByCustomerH
    DepositsParams{depositsSpent}
    depositsTxIdsLink
    mexpand
    (Down time)
    (customer, (_addr, ValueTransferC{received, spent}))
    widget
        | expand = do
            let trId = T.pack $ "customers-tx-ids-" <> show customer
            tr_
                [ id_ trId
                ]
                $ do
                    let spentColumn = if depositsSpent then succ else id
                        columns =
                            T.pack
                                $ show
                                $ spentColumn (2 :: Int)
                        bar =
                            div_ $ do
                                div_ $ toHtml $ "Cutomer: " <> show customer
                        close =
                            button_
                                [ class_ "btn p-1"
                                , type_ "button"
                                , hxTarget_ $ "#" <> trId
                                , hxSwap_ "outerHTML"
                                , hxPost_ $ txIdsPost (Just Collapse)
                                , hxInclude_ "#view-control"
                                ]
                                $ i_ [class_ "bi bi-x"] mempty
                    td_ [colspan_ columns, class_ "p-0"] $ box bar close $ do
                        widget [class_ "ps-1"]
                        input_
                            [ type_ "hidden"
                            , name_ "tx-ids"
                            , value_ $ toUrlPiece time
                            ]
        | otherwise = do
            let trId = T.pack $ "customers-tx-ids-" <> show customer
            tr_
                [ scope_ "row"
                , hxTrigger_ "click"
                , hxTarget_ "this"
                , hxSwap_ "outerHTML"
                , hxPost_ $ txIdsPost (Just Expand)
                , hxInclude_ "#view-control"
                , id_ trId
                ]
                $ do
                    tdEnd $ toHtml $ show customer
                    tdEnd $ valueH received
                    when depositsSpent
                        $ tdEnd
                        $ valueH spent
      where
        txIdsPost =
            linkText
                . depositsTxIdsLink
                    (Just time)
                    (Just customer)
        expand = Just Expand == mexpand
