module Cardano.Wallet.UI.Deposit.Server.Deposits.Times
    ( serveDepositsPagination
    , serveDeposits
    )
where

import Prelude

import Cardano.Slotting.Slot
    ( WithOrigin (..)
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
    , depositsTimesPaginatingLink
    )
import Cardano.Wallet.UI.Deposit.API.Deposits.Deposits
    ( DepositsParams (..)
    )
import Cardano.Wallet.UI.Deposit.Handlers.Deposits.Times
    ( depositsPaginateM
    )
import Cardano.Wallet.UI.Deposit.Handlers.Lib
    ( catchRunWalletResourceM
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Deposits.Times
    ( scrollableDeposits
    )
import Cardano.Wallet.UI.Deposit.Server.Lib
    ( renderSmoothHtml
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
    -> DepositsParams
    -> Maybe RequestCookies
    -> Handler (CookieResponse RawHtml)
serveDeposits ul params = withSessionLayer ul $ \layer -> do
    result <- catchRunWalletResourceM layer $ do
        scrolling <- depositsTable params
        pure $ widget scrolling []
    pure $ renderSmoothHtml result

depositsTable
    :: DepositsParams
    -> WalletResourceM (Scrolling WalletResourceM DownTime)
depositsTable params = do
    let hs =
            depositsPaginateM
                params
                getTxHistoryByTime
                100
    newScrolling
        $ scrollableDeposits
            depositsTimesPaginatingLink
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
