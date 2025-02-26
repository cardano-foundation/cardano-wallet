module Cardano.Wallet.UI.Shelley.Html.Pages.Wallets.NewWallet where

import Prelude hiding
    ( id
    )

import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxExt_
    , hxGet_
    , hxPost_
    , hxTarget_
    , useHtmxExtension
    )
import Lucid
    ( Html
    , ToHtml (..)
    , autocomplete_
    , button_
    , class_
    , div_
    , form_
    , id_
    , input_
    , name_
    , placeholder_
    , role_
    , type_
    )

import Cardano.Wallet.UI.Common.API
    ( Visible (..)
    )
import Cardano.Wallet.UI.Common.Html.Copy
    ( copyButton
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    )
import Cardano.Wallet.UI.Shelley.API
    ( walletLink
    , walletMnemonicLink
    )
import Data.Text
    ( Text
    )

import qualified Data.Text as T

mnemonicH :: Maybe [Text] -> Html ()
mnemonicH Nothing = ""
mnemonicH (Just mnemonic) = do
    div_ [class_ "card"] $ do
        div_
            [ class_ "card-body text-muted small"
            , id_ "copy-mnemonic"
            ]
            $ toHtml
            $ T.intercalate " " mnemonic
        copyButton "copy-mnemonic"

newWalletH :: Html ()
newWalletH = do
    useHtmxExtension "json-enc"
    div_ [class_ "btn-group mb-3", role_ "group"] $ do
        button_
            [ class_ "btn btn-outline-secondary"
            , hxGet_ $ linkText $ walletMnemonicLink Nothing
            , hxTarget_ "#menmonic"
            ]
            "Hint a mnemonic"
        button_
            [ class_ "btn btn-outline-secondary"
            , hxGet_ $ linkText $ walletMnemonicLink $ Just True
            , hxTarget_ "#menmonic"
            ]
            "Clean hinted mnemonic"

    div_ [id_ "menmonic", class_ "mb-3"] ""

    postWalletForm Nothing

    div_
        [ id_ "new_wallet"
        ]
        mempty

postWalletForm :: Maybe Visible -> Html ()
postWalletForm mv = form_
    [ hxPost_ $ linkText walletLink
    , hxExt_ "json-enc"
    , hxTarget_ "#new_wallet"
    , autocomplete_ "off"
    ]
    $ do
        input_
            [ class_ "form-control form-control-lg  m-1 p-1"
            , visibility
            , name_ "mnemonicSentence"
            , placeholder_ "Mnemonic Sentence"
            ]
        input_
            [ class_ "form-control form-control-lg m-1 p-1"
            , type_ "text"
            , name_ "name"
            , placeholder_ "Wallet Name"
            ]
        input_
            [ class_ "form-control form-control-lg m-1 p-1"
            , visibility
            , name_ "passphrase"
            , placeholder_ "Passphrase"
            ]
        button_
            [ class_ "btn btn-primary btn-block m-1 p-1"
            , type_ "submit"
            ]
            "Restore wallet"
  where
    visibility = type_ $ case mv of
        Just Visible -> "text"
        Just Hidden -> "password"
        Nothing -> "password"
