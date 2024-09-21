{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Addresses
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletPublicIdentity (..)
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Cardano.Wallet.UI.Common.Html.Copy
    ( copyButton
    , copyableHidden
    )
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxPost_
    , hxTarget_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( record
    , simpleField
    , sseH
    )
import Cardano.Wallet.UI.Deposit.API
    ( addressesLink
    , customerAddressLink
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( WalletPresent (..)
    )
import Cardano.Wallet.UI.Lib.Address
    ( encodeMainnetAddress
    )
import Cardano.Wallet.UI.Type
    ( WHtml
    )
import Data.Text.Class
    ( ToText (..)
    )
import Lucid
    ( Html
    , HtmlT
    , ToHtml (..)
    , class_
    , div_
    , id_
    , input_
    , min_
    , name_
    , type_
    , value_
    )
import Lucid.Html5
    ( max_
    , step_
    )

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

addressesH :: WHtml ()
addressesH = do
    sseH addressesLink "addresses" ["wallet"]

customerAddressH :: Monad m => Address -> HtmlT m ()
customerAddressH addr = div_ [class_ "d-flex justify-content-end"] $ do
    div_ (copyableHidden "address") $ toHtml encodedAddr
    div_ [class_ ""] $ toHtml addrShortened
    div_ [class_ "ms-1"] $ copyButton "address"
  where
    encodedAddr = encodeMainnetAddress addr
    addrShortened =
        T.take 10 (T.drop 5 encodedAddr)
            <> " .. "
            <> T.takeEnd 10 encodedAddr

addressElementH :: (BL.ByteString -> Html ()) -> WalletPresent -> Html ()
addressElementH alert = \case
    WalletPresent (WalletPublicIdentity _xpub customers) -> do
        div_ [class_ "row mt-5"] $ do
            div_ [class_ "col"] $ record $ do
                simpleField "Customer Number"
                    $ input_
                        [ type_ "number"
                        , hxTarget_ "#customer-address"
                        , class_ "form-control"
                        , hxTrigger_ "load, change"
                        , hxPost_ $ linkText customerAddressLink
                        , min_ "0"
                        , max_ $ toText $ customers - 1
                        , step_ "1"
                        , name_ "customer"
                        , value_ "0"
                        , class_ "w-3"
                        ]
                simpleField "Address" $ div_ [id_ "customer-address"] mempty
    WalletAbsent -> alert "Wallet is absent"
    WalletFailedToInitialize err ->
        alert
            $ "Failed to initialize wallet"
                <> BL.pack (show err)
    WalletVanished e -> alert $ "Wallet vanished " <> BL.pack (show e)
    WalletInitializing -> alert "Wallet is initializing"
    WalletClosing -> alert "Wallet is closing"
