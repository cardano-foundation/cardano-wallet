module Cardano.Wallet.UI.Deposit.Server.Deposits
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
    ( DepositsParams (..)
    , DownTime
    , Expand
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits
    ( depositCustomersHandler
    , depositCustomersPaginationHandlers
    , depositsPaginationHandlers
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits.Fake
    ( getFakeDepositsHistory
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits.TxIds
    ( depositCustomersTxIdsHandler
    , depositCustomersTxIdsPaginationHandlers
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceM
    , walletPresence
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Deposits
    ( depositByCustomerH
    , depositH
    , depositsElementH
    , scrollableDeposits
    , scrollableDepositsCustomers
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

serveDeposits
    :: UILayer WalletResource
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDeposits ul = withSessionLayer ul $ \layer -> do
    wp <- walletPresence layer
    pure
        $ renderSmoothHtml
        $ depositsElementH alertH wp

depositsTable
    :: DepositsParams
    -> WalletResourceM (Scrolling WalletResourceM DownTime)
depositsTable params = do
    let hs = depositsPaginationHandlers params getFakeDepositsHistory 100
    newScrolling $ scrollableDeposits params hs

serveDepositsHistoryPage
    :: UILayer WalletResource
    -> DepositsParams
    -> Maybe (WithOrigin UTCTime)
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDepositsHistoryPage ul params (Just index) = withSessionLayer ul
    $ \layer -> do
        result <- catchRunWalletResourceM layer $ do
            scrolling <- depositsTable params
            scroll scrolling (depositsPages params) $ Down index
        pure $ renderHtml result
serveDepositsHistoryPage ul _ _ = withSessionLayer ul
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
            depositCustomersPaginationHandlers
                params
                getFakeDepositsHistory
                time
                100
    newScrolling $ scrollableDepositsCustomers params time hs

depositsCustomersTxIdsTable
    :: DepositsParams
    -> DownTime
    -> Customer
    -> WalletResourceM (Scrolling WalletResourceM TxId)
depositsCustomersTxIdsTable params time customer = do
    let hs =
            depositCustomersTxIdsPaginationHandlers
                params
                getFakeDepositsHistory
                time
                customer
                100
    newScrolling $ scrollableDepositsCustomersTxIds params time customer hs

serveDepositsHistoryWindowPage
    :: UILayer WalletResource
    -> DepositsParams
    -> (Maybe (WithOrigin UTCTime))
    -> Maybe Customer
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDepositsHistoryWindowPage ul params (Just time) (Just customer) =
    withSessionLayer ul
        $ \layer -> do
            result <- catchRunWalletResourceM layer $ do
                scrolling <- depositsCustomersTable params (Down time)
                scroll scrolling (depositsCustomersPages params) customer
            pure $ renderHtml result
serveDepositsHistoryWindowPage ul _ _ _ = withSessionLayer ul
    $ \_layer -> do
        pure
            $ renderHtml
            $ alertH ("No time or customer provided" :: Text)

serveDepositsHistory
    :: UILayer WalletResource
    -> DepositsParams
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDepositsHistory ul params = withSessionLayer ul $ \layer -> do
    result <- catchRunWalletResourceM layer $ do
        scrolling <- depositsTable params
        pure $ widget scrolling []
    pure $ renderSmoothHtml result

serveDepositsHistoryWindow
    :: UILayer WalletResource
    -> DepositsParams
    -> Maybe (WithOrigin UTCTime)
    -> Maybe Expand
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDepositsHistoryWindow ul params mtime mexpand = withSessionLayer ul
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

serveDepositsCustomersTxIdsPage
    :: UILayer WalletResource
    -> DepositsParams
    -> Maybe (WithOrigin UTCTime)
    -> Maybe Customer
    -> Maybe TxId
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDepositsCustomersTxIdsPage ul params (Just time) (Just customer) (Just txId) =
    withSessionLayer ul
        $ \layer -> do
            result <- catchRunWalletResourceM layer $ do
                scrolling <- depositsCustomersTxIdsTable params (Down time) customer
                scroll scrolling (depositsCustomersTxIdsPages params) txId
            pure $ renderHtml result
serveDepositsCustomersTxIdsPage ul _ _ _ _ = withSessionLayer ul
    $ \_layer -> do
        pure
            $ renderHtml
            $ alertH ("No time, customer or txId provided" :: Text)
