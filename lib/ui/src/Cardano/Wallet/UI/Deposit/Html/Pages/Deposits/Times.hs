{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.Times
    ( depositH
    , scrollableDeposits
    )
where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( ValueTransfer (..)
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( DownTime
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
    ( downTimeH
    , valueH
    )
import Cardano.Wallet.UI.Lib.Discretization
    ( nextDiscretizedTime
    )
import Cardano.Wallet.UI.Lib.Pagination.Type
    ( Paginate (..)
    , PaginateM
    )
import Control.Monad
    ( when
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
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
import Data.Time
    ( UTCTime
    )
import Lucid
    ( Attribute
    , Html
    , ToHtml (..)
    , button_
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

scrollableDeposits
    :: MonadIO m
    => (Maybe (WithOrigin UTCTime) -> Link)
    -> (Maybe (WithOrigin UTCTime) -> Maybe Expand -> Link)
    -> DepositsParams
    -> PaginateM
        m
        DownTime
        (Map DownTime (Maybe Slot, ValueTransfer))
    -> Scrolling.Configuration m DownTime
scrollableDeposits
    depositsHistoryPageLink
    depositsCustomersLink
    params@DepositsParams{depositsSpent, depositsSlot}
    Paginate{nextIndex, previousIndex, pageAtIndex, minIndex} =
        Scrolling.Configuration{..}
      where
        scrollableWidget :: [Attribute] -> Html () -> Html ()
        scrollableWidget attrs content = do
            let attrs' =
                    [ class_ "border-top table table-striped table-hover m-0"
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
                            thEnd (Just 7) "Deposit"
                            when depositsSpent
                                $ thEnd (Just 7) "Spent"
                    content
        scrollableContainer = table_
        retrieveContent time attrs = do
            mds <- pageAtIndex time
            case mds of
                Nothing -> pure $ toHtml $ "No deposits for index " <> show time
                Just (_, ds) ->
                    pure
                        $ tbody_ attrs
                        $ mapM_
                            ( \window ->
                                depositH
                                    params
                                    Nothing
                                    depositsCustomersLink
                                    window
                                    mempty
                            )
                        $ Map.assocs ds
        uniqueScrollingId = "deposits"
        presentFieldName = "times-paginating-presence"
        controlSelector = "#view-control"
        renderIndex (Down t) = toUrlPiece t
        updateURL (Down t) = linkText . depositsHistoryPageLink . Just $ t
        renderIdOfIndex (Down t) = toUrlPiece $ abs $ hash t

depositH
    :: DepositsParams
    -> Maybe Expand
    -> (Maybe (WithOrigin UTCTime) -> Maybe Expand -> Link)
    -> (DownTime, (Maybe Slot, ValueTransfer))
    -> ([Attribute] -> Html ())
    -> Html ()
depositH
    DepositsParams
        { depositsSlot
        , depositsSpent
        , depositsCustomers
        , depositsWindow
        , depositsFirstWeekDay
        }
    mexpand
    depositsCustomersLink
    (dTime@(Down time), (slot, ValueTransferC{received, spent}))
    widget
        | expanded = do
            let
                trId = T.pack $ "customers-" <> show (hash time)
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
                        -- This number refers to the number of columns
                        -- in the container table.
                        bar = do
                            div_
                                $ toHtml
                                $ "From: "
                                    <> downTimeH
                                        ( fmap
                                            ( nextDiscretizedTime
                                                depositsFirstWeekDay
                                                depositsWindow
                                            )
                                            <$> dTime
                                        )
                            div_
                                $ toHtml
                                $ "To: " <> downTimeH dTime
                        close =
                            button_
                                [ class_ "btn p-1"
                                , type_ "button"
                                , hxTarget_ $ "#" <> trId
                                , hxSwap_ "outerHTML"
                                , hxPost_ $ customerPost $ Just Collapse
                                , hxInclude_ "#view-control"
                                ]
                                $ i_ [class_ "bi bi-x"] mempty
                    td_ [colspan_ columns, class_ "p-0"] $ box bar close $ do
                        widget [class_ "ps-1"]
                        input_
                            [ type_ "hidden"
                            , name_ "customers"
                            , value_ $ toUrlPiece time
                            ]
        | otherwise = do
            tr_
                [ scope_ "row"
                , hxTrigger_ "click"
                , hxTarget_ "this"
                , hxSwap_ "outerHTML"
                , hxPost_ $ customerPost (Just Expand)
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
        customerPost mexpand' =
            linkText $ depositsCustomersLink (Just time) mexpand'
        expanded = maybe (time `elem` depositsCustomers) (== Expand) mexpand
