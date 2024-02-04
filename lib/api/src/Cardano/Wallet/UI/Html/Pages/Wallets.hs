{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Html.Pages.Wallets where

import Prelude hiding
    ( id
    )

import Cardano.Wallet.Api.Types
    ( ApiT (..)
    , ApiWallet (..)
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
    , class_
    , scope_
    )

import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Cardano.Wallet.UI.API
    ( walletsListLink
    )
import Cardano.Wallet.UI.Html.Pages.Lib
    ( AssocRow
    , recordTable
    , row
    , sseH
    )
import Cardano.Wallet.UI.Html.Pages.ListOf
    ( ListOf
    )
import Cardano.Wallet.UI.Html.Pages.Wallet
    ( renderBalance
    , renderState
    )

walletsH :: Html ()
walletsH = sseH walletsListLink "content" ["refresh", "wallets"]

walletListH :: Maybe WalletId -> [(ApiWallet, UTCTime)] -> Html ()
walletListH mwid wallets = recordTable
    $ forM_ wallets
    $ \(w, _) -> do
        let wid' = getApiT $ id w
        walletElementH w
            $ [ scope_ "col"
              , hxTrigger_ "click"
              , hxPost_ $ "/settings/wallet/select/" <> toText wid'
              , hxSwap_ "none"
              ]
                <> [class_ "table-primary" | mwid == Just wid']

walletElementH :: ApiWallet -> [Attribute] -> ListOf AssocRow
walletElementH ApiWallet{..} attrs =
    row attrs (toText $ getApiT name) $ do
        recordTable $ do
            row [] "id" $ toText $ getApiT id
            row [] "state" $ renderState state
            row [] "balance" $ renderBalance balance
