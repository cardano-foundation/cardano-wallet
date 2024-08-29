{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Personal.Html.Pages.Wallets where

import Prelude hiding
    ( id
    )

import Cardano.Wallet.Api.Types
    ( ApiT (..)
    , ApiWallet (..)
    , ApiWalletBalance (..)
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxPost_
    , hxSwap_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( AssocRow
    , field
    , record
    , simpleField
    , sseH
    )
import Cardano.Wallet.UI.Lib.ListOf
    ( ListOf
    )
import Cardano.Wallet.UI.Personal.API
    ( settingsWalletSelectLink
    , sseLink
    , walletsListLink
    )
import Cardano.Wallet.UI.Personal.Html.Pages.Wallet
    ( renderState
    )
import Cardano.Wallet.UI.Personal.Html.Pages.Wallets.NewWallet
    ( newWalletH
    )
import Control.Monad
    ( forM_
    )
import Data.Text
    ( Text
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
    , i_
    , scope_
    )

data Selected = Selected | NotSelected

walletsH :: Html ()
walletsH = do
    sseH sseLink walletsListLink "content" ["wallets"]
    newWalletH

walletListH :: Maybe WalletId -> [(ApiWallet, UTCTime)] -> Html ()
walletListH mwid wallets = record
    $ forM_ wallets
    $ \(w, _) -> do
        let wid' = getApiT $ id w
        walletElementH
            (mkSelected $ mwid == Just wid')
            w
            [ scope_ "col"
            , hxTrigger_ "click"
            , hxPost_ $ linkText $ settingsWalletSelectLink wid'
            , hxSwap_ "none"
            ]

selectedName :: Selected -> Text -> Html ()
selectedName Selected name = toHtml name >> checked
selectedName NotSelected name = toHtml name

checked :: Html ()
checked =
    i_
        [ class_ "bi bi-check2 ml-1 h-4 test-checked"
        ]
        $ pure ()

mkSelected :: Bool -> Selected
mkSelected True = Selected
mkSelected False = NotSelected

walletElementH :: Selected -> ApiWallet -> [Attribute] -> ListOf AssocRow
walletElementH selected ApiWallet{..} attrs =
    field attrs (selectedName selected $ toText $ getApiT name) $ do
        record $ do
            simpleField "id" $ toText $ getApiT id
            simpleField "state" $ renderState state
            simpleField "balance" $ renderBalance balance

renderBalance :: ApiWalletBalance -> Html ()
renderBalance ApiWalletBalance{..} = toHtml $ toText available
