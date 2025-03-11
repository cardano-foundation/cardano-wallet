{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Shelley.Html.Pages.Wallets where

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
import Cardano.Wallet.UI.Common.API
    ( Visible (..)
    )
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxPost_
    , hxSwap_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    , toTextHtml
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( AssocRow
    , Striped (..)
    , Width (..)
    , field
    , record
    , simpleField
    )
import Cardano.Wallet.UI.Common.Html.Pages.Wallet
    ( PostWalletConfig (..)
    , newWalletFromMnemonicH
    )
import Cardano.Wallet.UI.Lib.ListOf
    ( ListOf
    )
import Cardano.Wallet.UI.Shelley.API
    ( settingsWalletSelectLink
    , walletLink
    , walletMnemonicLink
    )
import Cardano.Wallet.UI.Shelley.Html.Pages.Wallet
    ( renderState
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
    , HtmlT
    , ToHtml (..)
    , class_
    , div_
    , i_
    , id_
    , scope_
    )

data Selected = Selected | NotSelected

walletsH :: Html ()
walletsH = do
    -- sseH sseLink walletsListLink "content" ["wallets"]
    newWalletFromMnemonicH walletMnemonicLink
        $ PostWalletConfig
            { walletDataLink = walletLink
            , passwordVisibility = Just Hidden
            , responseTarget = "#post-response"
            }
    div_ [id_ "#post-response"] mempty

walletListH :: Maybe WalletId -> [(ApiWallet, UTCTime)] -> Html ()
walletListH mwid wallets = record Nothing Full Striped
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

selectedName :: Monad m => Selected -> Text -> HtmlT m ()
selectedName Selected name = toHtml name >> checked
selectedName NotSelected name = toHtml name

checked :: Applicative m => HtmlT m ()
checked =
    i_
        [ class_ "bi bi-check2 ml-1 h-4 test-checked"
        ]
        $ pure ()

mkSelected :: Bool -> Selected
mkSelected True = Selected
mkSelected False = NotSelected

walletElementH
    :: Monad m
    => Selected
    -> ApiWallet
    -> [Attribute]
    -> ListOf (AssocRow m)
walletElementH selected ApiWallet{..} attrs =
    field attrs (selectedName selected $ toText $ getApiT name) $ do
        record Nothing Full Striped  $ do
            simpleField "id" $ toTextHtml $ getApiT id
            simpleField "state" $ toHtml $ renderState state
            simpleField "balance" $ renderBalance balance

renderBalance :: Monad m => ApiWalletBalance -> HtmlT m ()
renderBalance ApiWalletBalance{..} = toHtml $ toText available
