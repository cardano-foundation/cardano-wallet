{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Html.Pages.Wallets where

import Prelude hiding
    ( id
    )

import Cardano.Wallet.Api.Types
    ( ApiT (..)
    , ApiWallet (..)
    , ApiWalletBalance (..)
    )
import Cardano.Wallet.UI.Html.Htmx
    ( hxPost_
    , hxSwap_
    , hxTrigger_
    )
import Control.Monad
    ( forM_
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time
    ( UTCTime
    )
import Lucid
    ( Attribute
    , Html
    , ToHtml (..)
    , class_
    , scope_
    )

import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Cardano.Wallet.UI.API
    ( linkText
    , settingsWalletSelectLink
    , sseLink
    , walletsListLink
    )
import Cardano.Wallet.UI.Html.Pages.Lib
    ( AssocRow
    , field
    , simpleField
    , record
    , sseH
    )
import Cardano.Wallet.UI.Html.Pages.Wallet
    ( renderState
    )
import Cardano.Wallet.UI.Html.Pages.Wallets.NewWallet
    ( newWalletH
    )
import Cardano.Wallet.UI.Lib.ListOf
    ( ListOf
    )

walletsH :: Html ()
walletsH = do
    sseH sseLink walletsListLink "content" ["refresh", "wallets"]
    newWalletH

walletListH :: Maybe WalletId -> [(ApiWallet, UTCTime)] -> Html ()
walletListH mwid wallets = record
    $ forM_ wallets
    $ \(w, _) -> do
        let wid' = getApiT $ id w
        walletElementH w
            $ [ scope_ "col"
              , hxTrigger_ "click"
              , hxPost_ $ linkText $ settingsWalletSelectLink wid'
              , hxSwap_ "none"
              ]
                <> [class_ "table-primary" | mwid == Just wid']

walletElementH :: ApiWallet -> [Attribute] -> ListOf AssocRow
walletElementH ApiWallet{..} attrs =
    field attrs (toText $ getApiT name) $ do
        record $ do
            simpleField "id" $ toText $ getApiT id
            simpleField "state" $ renderState state
            simpleField "balance" $ renderBalance balance

renderBalance :: ApiWalletBalance -> Html ()
renderBalance ApiWalletBalance{..} =  toHtml $ toText available
