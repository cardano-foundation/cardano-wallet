{-# LANGUAGE RecordWildCards #-}

module Cardano.Wallet.UI.Common.Html.Pages.Wallet
where

import Cardano.Wallet.UI.Common.API
    ( Visible (..)
    )
import Cardano.Wallet.UI.Common.Html.Copy
    ( copyButton
    )
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxGet_
    , hxInclude_
    , hxPost_
    , hxTarget_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( box
    )
import Data.Text
    ( Text
    )
import Lucid
    ( Attribute
    , Html
    , ToHtml (..)
    , autocomplete_
    , button_
    , class_
    , div_
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
import Prelude

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
postWalletFormTagH :: Text -> PostWalletConfig -> Html () -> Html ()
postWalletFormTagH idName PostWalletConfig{} =
    div_
        [ autocomplete_ "off"
        , class_ "p-2"
        , id_ idName
        ]

--------------------------------------------------------------------------------
-- Wallet creation forms
--------------------------------------------------------------------------------

-- | Widget to create a new wallet from a mnemonic
newWalletFromMnemonicH
    :: (Maybe Bool -> Link) -> PostWalletConfig -> Html ()
newWalletFromMnemonicH walletMnemonicLink config = do
    box "Restore from Mnemonic" mempty $ do
        div_ [class_ "p-2"] $ do
            postWalletFormTagH "wallet-from-menmonic" config
                $ mnemonicSetupFieldsH walletMnemonicLink config

-- | Display a mnemonic
mnemonicH :: Maybe [Text] -> Html ()
mnemonicH Nothing = ""
mnemonicH (Just mnemonic) = do
    div_ [class_ "d-flex justify-content-end align-items-center"] $ do
        div_
            [ class_ "p-2"
            , id_ "copy-mnemonic"
            ]
            $ toHtml
            $ T.intercalate " " mnemonic
        copyButton "copy-mnemonic"

-- | Form fields for restoring a wallet from a mnemonic
mnemonicSetupFieldsH
    :: (Maybe Bool -> Link) -> PostWalletConfig -> Html ()
mnemonicSetupFieldsH walletMnemonicLink PostWalletConfig{..} = do
    div_ [class_ "border-start"] $ do
        div_ [class_ "d-flex justify-content-end align-items-center"]
            $ div_ [class_ "btn-group", role_ "group"]
            $ do
                button_
                    [ class_ "btn btn-outline-secondary"
                    , hxGet_ $ linkText $ walletMnemonicLink $ Just False
                    , hxTarget_ "#menmonic"
                    ]
                    "Hint"
                button_
                    [ class_ "btn btn-outline-secondary"
                    , hxGet_ $ linkText $ walletMnemonicLink $ Just True
                    , hxTarget_ "#menmonic"
                    ]
                    "Clean"
        div_ [id_ "menmonic", class_ "mb"] ""
        input_
            [ formControl
            , visibility
            , name_ "mnemonics"
            , placeholder_ "Mnemonic Sentence"
            ]
    input_
        [ formControl
        , type_ "text"
        , name_ "name"
        , placeholder_ "Wallet Name"
        ]
    input_
        [ formControl
        , type_ "password"
        , name_ "password"
        , placeholder_ "Signing Passphrase"
        ]
    div_ [class_ "d-flex justify-content-end align-items-center"] $ do
        button_
            [ class_ "btn btn-primary btn-block mb-3"
            , type_ "submit"
            , hxPost_ $ linkText walletDataLink
            , hxTarget_ responseTarget
            , hxInclude_ "#wallet-from-menmonic"
            ]
            "Restore"
  where
    visibility = type_ $ case passwordVisibility of
        Just Visible -> "text"
        Just Hidden -> "password"
        Nothing -> "password"

--------------------------------------------------------------------------------
-- Wallet restoration from public key
--------------------------------------------------------------------------------

newWalletFromXPubH :: PostWalletConfig -> Html ()
newWalletFromXPubH config@PostWalletConfig{..} = do
    box "Restore from Public Key" mempty $ div_ [class_ "p-2"] $ do
        postWalletFormTagH "wallet-from-xpub" config $ do
            input_
                [ formControl
                , type_ "text"
                , name_ "xpub"
                , placeholder_ "Extended Public Key"
                ]
            div_ [class_ "d-flex justify-content-end align-items-center"] $ do
                button_
                    [ class_ "btn btn-primary btn-block mb-3"
                    , hxPost_ $ linkText walletDataLink
                    , hxTarget_ responseTarget
                    , hxInclude_ "#wallet-from-xpub"
                    ]
                    "Restore"

formControl :: Attribute
formControl = class_ "form-control m-1 px-3 py-"
