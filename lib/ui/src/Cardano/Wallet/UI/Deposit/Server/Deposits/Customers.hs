module Cardano.Wallet.UI.Deposit.Server.Deposits.Customers
    ( serveDepositsCustomers
    , serveDepositsCustomerPagination
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
    ( depositsCustomersLink
    , depositsCustomersPaginatingLink
    , depositsTxIdsLink
    )
import Cardano.Wallet.UI.Deposit.API.Common
    ( Expand
    )
import Cardano.Wallet.UI.Deposit.API.Deposits.Deposits
    ( DepositsParams (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits.Customers
    ( depositCustomersHandler
    , depositCustomersPaginateM
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceM
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.Customers
    ( scrollableDepositsCustomers
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.Times
    ( depositH
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

depositsCustomersTable
    :: DepositsParams
    -> DownTime
    -> WalletResourceM (Scrolling WalletResourceM Customer)
depositsCustomersTable params time = do
    let hs =
            depositCustomersPaginateM
                params
                getTxHistoryByTime
                time
                100
    newScrolling
        $ scrollableDepositsCustomers
            params
            depositsCustomersPaginatingLink
            depositsTxIdsLink
            time
            hs

serveDepositsCustomerPagination
    :: UILayer WalletResource
    -> DepositsParams
    -> (Maybe (WithOrigin UTCTime))
    -> Maybe Customer
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDepositsCustomerPagination ul params (Just time) (Just customer) =
    withSessionLayer ul
        $ \layer -> do
            result <- catchRunWalletResourceM layer $ do
                scrolling <- depositsCustomersTable params (Down time)
                scroll scrolling (depositsCustomersPages params) customer
            pure $ renderHtml result
serveDepositsCustomerPagination ul _ _ _ = withSessionLayer ul
    $ \_layer -> do
        pure
            $ renderHtml
            $ alertH ("No time or customer provided" :: Text)

serveDepositsCustomers
    :: UILayer WalletResource
    -> DepositsParams
    -> Maybe (WithOrigin UTCTime)
    -> Maybe Expand
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDepositsCustomers ul params mtime mexpand = withSessionLayer ul
    $ \layer -> do
        fmap renderHtml $ case mtime of
            Nothing -> pure $ alertH ("No time provided" :: Text)
            Just time -> do
                result <- catchRunWalletResourceM layer $ do
                    scrolling <- depositsCustomersTable params (Down time)
                    pure $ widget scrolling
                depositCustomersHandler
                    layer
                    ( \window ->
                        depositH
                            params
                            mexpand
                            depositsCustomersLink
                            ( Down time
                            , first getFirst
                                $ fold
                                $ unPatch window
                            )
                            result
                    )
                    alertH
                    params
                    time
