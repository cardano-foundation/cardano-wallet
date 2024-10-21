{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.Customers
    ( scrollableDepositsCustomers
    )
where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( Customer
    , ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    , tdEnd
    , thEnd
    )
import Cardano.Wallet.UI.Deposit.API
    ( DepositsParams (..)
    , DownTime
    , depositsHistoryWindowPageLink
    )
import Cardano.Wallet.UI.Deposit.Handlers.Pagination
    ( PaginationHandlers (..)
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

scrollableDepositsCustomers
    :: Monad m
    => DepositsParams
    -> DownTime
    -> PaginationHandlers m Customer (Map Customer (Maybe Address, ValueTransfer))
    -> Scrolling.Configuration m Customer
scrollableDepositsCustomers
    params@DepositsParams{depositsSpent, depositsFakeData}
    (Down time)
    PaginationHandlers{previousPageIndex, nextPageIndex, retrievePage, startingIndex} =
        Scrolling.Configuration{..}
      where
        previous = previousPageIndex
        -- fmap snd <$> previousH (time, c)
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
                                thEnd Nothing "Addr"
                                thEnd (Just 6) "Customer"
                                thEnd (Just 7) "Received"
                                when depositsSpent
                                    $ thEnd (Just 7) "Spent"
                        content
          where
            fakeOverlay = if depositsFakeData then overlayFakeDataH else id
        scrollableContainer = table_
        retrieveContent customer attrs = do
            xs <- retrievePage customer
            pure
                $ tbody_ attrs
                $ mapM_ (depositByAddressH params)
                $ sortOn (view _1)
                $ Map.assocs xs
        uniqueScrollingId = "deposit-customers"
        presentFieldName = "customers-page-present"
        controlSelector = "#view-control"
        renderIndex = toUrlPiece
        updateURL c =
            linkText
                $ depositsHistoryWindowPageLink
                    (Just time)
                    (Just c)
        renderIdOfIndex = T.replace " " "-" . toUrlPiece

depositByAddressH
    :: DepositsParams
    -> (Customer, (Maybe Address, ValueTransfer))
    -> Html ()
depositByAddressH
    DepositsParams{depositsSpent}
    (customer, (addr, ValueTransfer received spent)) = do
        tr_ [scope_ "row"] $ do
            tdEnd $ maybe "" customerAddressH addr
            tdEnd $ toHtml $ show customer
            tdEnd $ valueH received
            when depositsSpent
                $ tdEnd
                $ valueH spent
