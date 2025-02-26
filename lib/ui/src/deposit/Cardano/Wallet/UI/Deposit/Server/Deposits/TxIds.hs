module Cardano.Wallet.UI.Deposit.Server.Deposits.TxIds
    ( serveDepositsCustomersTxIds
    , serveDepositsCustomersTxIdsPagination
    )
where

import Prelude

import Cardano.Slotting.Slot
    ( WithOrigin (..)
    )
import Cardano.Wallet.Deposit.Map
    ( unPatch
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    )
import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( DownTime
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , WalletResourceM
    , getTxHistoryByTime
    )
import Cardano.Wallet.Read
    ( TxId
    )
import Cardano.Wallet.UI.Common.Handlers.Session
    ( withSessionLayer
    )
import Cardano.Wallet.UI.Common.Html.Html
    ( RawHtml (..)
    , renderHtml
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( alertH
    )
import Cardano.Wallet.UI.Common.Html.Scrolling
    ( Scrolling (..)
    , newScrolling
    )
import Cardano.Wallet.UI.Common.Layer
    ( UILayer (..)
    )
import Cardano.Wallet.UI.Cookies
    ( CookieResponse
    , RequestCookies
    )
import Cardano.Wallet.UI.Deposit.API
    ( depositsTxIdsLink
    )
import Cardano.Wallet.UI.Deposit.API.Common
    ( Expand
    )
import Cardano.Wallet.UI.Deposit.API.Deposits.Deposits
    ( DepositsParams (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits.TxIds
    ( depositCustomersTxIdsHandler
    , depositCustomersTxIdsPaginateM
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceM
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.Customers
    ( depositByCustomerH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.TxIds
    ( scrollableDepositsCustomersTxIds
    )
import Data.Bifunctor
    ( first
    )
import Data.Foldable
    ( fold
    )
import Data.Monoid
    ( First (..)
    )
import Data.Ord
    ( Down (..)
    )
import Data.Text
    ( Text
    )
import Data.Time
    ( UTCTime
    )
import Servant
    ( Handler
    )

depositsCustomersTxIdsTable
    :: DepositsParams
    -> DownTime
    -> Customer
    -> WalletResourceM (Scrolling WalletResourceM TxId)
depositsCustomersTxIdsTable params time customer = do
    let hs =
            depositCustomersTxIdsPaginateM
                params
                getTxHistoryByTime
                time
                customer
                100
    newScrolling
        $ scrollableDepositsCustomersTxIds params time customer hs

serveDepositsCustomersTxIdsPagination
    :: UILayer WalletResource
    -> DepositsParams
    -> Maybe (WithOrigin UTCTime)
    -> Maybe Customer
    -> Maybe TxId
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDepositsCustomersTxIdsPagination ul params (Just time) (Just customer) (Just txId) =
    withSessionLayer ul
        $ \layer -> do
            result <- catchRunWalletResourceM layer $ do
                scrolling <- depositsCustomersTxIdsTable params (Down time) customer
                scroll scrolling (depositsCustomersTxIdsPages params) txId
            pure $ renderHtml result
serveDepositsCustomersTxIdsPagination ul _ _ _ _ = withSessionLayer ul
    $ \_layer -> do
        pure
            $ renderHtml
            $ alertH ("No time, customer or txId provided" :: Text)

serveDepositsCustomersTxIds
    :: UILayer WalletResource
    -> DepositsParams
    -> Maybe (WithOrigin UTCTime)
    -> Maybe Customer
    -> Maybe Expand
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDepositsCustomersTxIds ul params (Just time) (Just customer) mexpand =
    withSessionLayer ul $ \layer -> do
        fmap renderHtml $ do
            result <- catchRunWalletResourceM layer $ do
                scrolling <- depositsCustomersTxIdsTable params (Down time) customer
                pure $ widget scrolling
            depositCustomersTxIdsHandler
                layer
                ( \txIds ->
                    depositByCustomerH
                        params
                        depositsTxIdsLink
                        mexpand
                        (Down time)
                        ( customer
                        , first getFirst
                            $ fold
                            $ unPatch txIds
                        )
                        result
                )
                alertH
                params
                time
                customer
serveDepositsCustomersTxIds ul _ _ _ _ = withSessionLayer ul
    $ \_layer -> do
        pure
            $ renderHtml
            $ alertH ("No time or customer provided" :: Text)
