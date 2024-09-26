{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Common.Html.Pages.Wallet
where

import Prelude

import Cardano.Wallet.UI.Common.API
    ( Visible (..)
    )
import Cardano.Wallet.UI.Common.Html.Copy
    ( copyButton
    )
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxGet_
    , hxPost_
    , hxTarget_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    )
import Cardano.Wallet.UI.Type
    ( WHtml
    , onDeposit
    , onShelley
    )
import Data.Text
    ( Text
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
import Servant
    ( Link
    )

import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for the wallet creation form
data PostWalletConfig = PostWalletConfig
    { passwordVisibility :: Maybe Visible
    -- ^ Whether the password should be visible
    , walletDataLink :: Link
    -- ^ Link to post the form data to
    , responseTarget :: Text
    }

--------------------------------------------------------------------------------
-- Library
--------------------------------------------------------------------------------

-- | Add a form tag with the appropriate attributes for a POST request
postWalletFormTagH :: PostWalletConfig -> WHtml () -> WHtml ()
postWalletFormTagH PostWalletConfig{..} =
    form_
        [ hxPost_ $ linkText walletDataLink
        , hxTarget_ responseTarget
        , autocomplete_ "off"
        ]

--------------------------------------------------------------------------------
-- Wallet creation forms
--------------------------------------------------------------------------------

-- | Widget to create a new wallet from a mnemonic
newWalletFromMnemonicH :: (Maybe Bool -> Link) -> PostWalletConfig -> WHtml ()
newWalletFromMnemonicH walletMnemonicLink config = do
    div_ [class_ "btn-group mb-3", role_ "group"] $ do
        button_
            [ class_ "btn btn-outline-secondary"
            , hxGet_ $ linkText $ walletMnemonicLink $ Just False
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

    postWalletFormTagH config $ mnemonicSetupFieldsH config

-- | Display a mnemonic
mnemonicH :: Maybe [Text] -> Html ()
mnemonicH Nothing = ""
mnemonicH (Just mnemonic) = do
    div_  [class_ "d-flex justify-content-end"] $ do
        div_
            [ class_ ""
            , id_ "copy-mnemonic"
            ]
            $ toHtml
            $ T.intercalate " " mnemonic
        copyButton "copy-mnemonic"

-- | Form fields for restoring a wallet from a mnemonic
mnemonicSetupFieldsH :: PostWalletConfig -> WHtml ()
mnemonicSetupFieldsH PostWalletConfig{..} = do
    input_
        [ class_ "form-control form-control-lg mb-3"
        , visibility
        , name_ "mnemonics"
        , placeholder_ "Mnemonic Sentence"
        ]
    onDeposit
        $ input_
            [ class_ "form-control form-control-lg mb-3"
            , type_ "number"
            , name_ "users"
            , placeholder_ "Customer Discovery"
            ]
    onShelley
        $ input_
            [ class_ "form-control form-control-lg mb-3"
            , type_ "text"
            , name_ "name"
            , placeholder_ "Wallet Name"
            ]
    onShelley
        $ input_
            [ class_ "form-control form-control-lg mb-3"
            , type_ "password"
            , name_ "password"
            , placeholder_ "Wallet Password"
            ]
    button_
        [ class_ "btn btn-primary btn-block mb-3"
        , type_ "submit"
        ]
        "Restore wallet from mnemonic"
  where
    visibility = type_ $ case passwordVisibility of
        Just Visible -> "text"
        Just Hidden -> "password"
        Nothing -> "password"

--------------------------------------------------------------------------------
-- Wallet restoration from public key
--------------------------------------------------------------------------------

newWalletFromXPubH :: PostWalletConfig -> WHtml ()
newWalletFromXPubH config = do
    postWalletFormTagH config $ do
        input_
            [ class_ "form-control form-control-lg mb-3"
            , type_ "text"
            , name_ "xpub"
            , placeholder_ "Extended Public Key"
            ]
        onDeposit
            $ input_
                [ class_ "form-control form-control-lg mb-3"
                , type_ "number"
                , name_ "users"
                , placeholder_ "Customer Discovery"
                ]
        button_
            [ class_ "btn btn-primary btn-block mb-3"
            , type_ "submit"
            ]
            "Restore wallet from public key"
