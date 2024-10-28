module Cardano.Wallet.UI.Deposit.Server.Deposits
    ( serveDepositsPage
    , serveDeposits
    , serveDepositsPagination
    , serveDepositsCustomers
    , serveDepositsCustomerPagination
    , serveDepositsCustomersTxIds
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
    , byTime
    )
import Cardano.Wallet.Deposit.REST
    ( WalletResource
    , WalletResourceM
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
    ( depositsCustomersLink
    , depositsCustomersPaginatingLink
    , depositsDepositsLink
    , depositsDepositsPaginatingLink
    , depositsTxIdsLink
    , fakeDataBackgroundLink
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
import Cardano.Wallet.UI.Deposit.Handlers.Deposits.Deposits
    ( depositsPaginateM
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits.Mock
    ( getMockHistory
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits.TxIds
    ( depositCustomersTxIdsHandler
    , depositCustomersTxIdsPaginateM
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceM
    , walletPresence
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.Customers
    ( depositByCustomerH
    , scrollableDepositsCustomers
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.Deposits
    ( depositH
    , scrollableDeposits
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.Page
    ( depositsElementH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.TxIds
    ( scrollableDepositsCustomersTxIds
    )
import Cardano.Wallet.UI.Deposit.Server.Lib
    ( renderSmoothHtml
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

serveDepositsPage
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDepositsPage ul = withSessionLayer ul $ \layer -> do
    wp <- walletPresence layer
    pure
        $ renderSmoothHtml
        $ depositsElementH depositsDepositsLink alertH wp

depositsTable
    :: DepositsParams
    -> WalletResourceM (Scrolling WalletResourceM DownTime)
depositsTable params = do
    let hs =
            depositsPaginateM
                params
                (byTime <$> getMockHistory)
                100
    newScrolling
        $ scrollableDeposits
            fakeDataBackgroundLink
            depositsDepositsPaginatingLink
            depositsCustomersLink
            params
            hs

serveDepositsPagination
    :: UILayer WalletResource
    -> DepositsParams
    -> Maybe (WithOrigin UTCTime)
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDepositsPagination ul params (Just index) = withSessionLayer ul
    $ \layer -> do
        result <- catchRunWalletResourceM layer $ do
            scrolling <- depositsTable params
            scroll scrolling (depositsPages params) $ Down index
        pure $ renderHtml result
serveDepositsPagination ul _ _ = withSessionLayer ul
    $ \_layer -> do
        pure
            $ renderHtml
            $ alertH ("No page index provided" :: Text)

depositsCustomersTable
    :: DepositsParams
    -> DownTime
    -> WalletResourceM (Scrolling WalletResourceM Customer)
depositsCustomersTable params time = do
    let hs =
            depositCustomersPaginateM
                params
                (byTime <$> getMockHistory)
                time
                100
    newScrolling
        $ scrollableDepositsCustomers
            params
            depositsCustomersPaginatingLink
            depositsTxIdsLink
            time
            hs

depositsCustomersTxIdsTable
    :: DepositsParams
    -> DownTime
    -> Customer
    -> WalletResourceM (Scrolling WalletResourceM TxId)
depositsCustomersTxIdsTable params time customer = do
    let hs =
            depositCustomersTxIdsPaginateM
                params
                (byTime <$> getMockHistory)
                time
                customer
                100
    newScrolling
        $ scrollableDepositsCustomersTxIds params time customer hs

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

serveDeposits
    :: UILayer WalletResource
    -> DepositsParams
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDeposits ul params = withSessionLayer ul $ \layer -> do
    result <- catchRunWalletResourceM layer $ do
        scrolling <- depositsTable params
        pure $ widget scrolling []
    pure $ renderSmoothHtml result

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
