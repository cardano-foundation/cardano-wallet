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
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxPost_
    , hxTarget_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( linkText
    , truncatableText
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
    , onWalletPresentH
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
    , style_
    , type_
    , value_
    )
import Lucid.Html5
    ( max_
    , size_
    , step_
    )

import qualified Data.ByteString.Lazy.Char8 as BL

addressesH :: WHtml ()
addressesH = do
    sseH addressesLink "addresses" ["wallet"]

customerAddressH :: Monad m => Address -> HtmlT m ()
customerAddressH addr = truncatableText "address-text" $ toHtml encodedAddr
  where
    encodedAddr = encodeMainnetAddress addr

addressElementH :: (BL.ByteString -> Html ()) -> WalletPresent -> Html ()
addressElementH = onWalletPresentH $ \case
    (WalletPublicIdentity _xpub customers) -> do
        div_ [class_ "row mt-5"] $ do
            div_ [class_ "col"] $ record (Just 11) $ do
                simpleField "Customer Number"
                    $ div_ [class_ "d-flex justify-content-end"]
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
                        , size_ "5"
                        , style_ "width: 7em"
                        ]
                simpleField "Address"
                    $ div_
                        [ id_ "customer-address"
                        ]
                        mempty
