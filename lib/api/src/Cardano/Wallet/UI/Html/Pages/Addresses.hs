{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Html.Pages.Addresses where

import Prelude hiding
    ( id
    )

import Cardano.Wallet.Address.Encoding
    ( encodeAddress
    )
import Cardano.Wallet.Api.Types
    ( ApiAddressWithPath (..)
    , ApiT (..)
    , apiAddress
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address
    )
import Cardano.Wallet.UI.API
    ( sseLink
    , walletAddressesLink
    )
import Cardano.Wallet.UI.Html.Pages.Lib
    ( copyButton
    , fieldHtml
    , record
    , simpleField
    , sseH
    )
import Control.Monad
    ( forM_
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text.Class
    ( ToText (..)
    )
import Lucid
    ( Html
    , ToHtml (toHtml)
    , class_
    , div_
    , id_
    , li_
    , ul_
    )

addressesPageH :: Html ()
addressesPageH =
        sseH sseLink walletAddressesLink "addresses" ["refresh", "wallet"]

addressesH :: forall n. HasSNetworkId n => [ApiAddressWithPath n] -> Html ()
addressesH addresses = record $ do
    forM_ (zip [0 :: Int ..] addresses) $ \(j, ApiAddressWithPath{..}) -> do
        fieldHtml [] "id" $ do
            let identifier =  "address-" <> toText j
            div_ [class_ "row"] $ do
                div_ [class_ "text-break col-sm-10" , id_ identifier]
                    $ addressH (Proxy @n)
                    $ apiAddress id
                div_ [class_ "col-sm-2"] $ copyButton identifier
        simpleField "state" $ toText $ getApiT state
        fieldHtml [] "derivation path"
            $ ul_ [class_ "list-inline"]
            $ forM_ derivationPath
            $ li_ [class_ "list-inline-item"]
                . toHtml
                . toText
                . getApiT

addressH :: forall n. HasSNetworkId n => Proxy n -> Address -> Html ()
addressH _ a = toHtml $ encodeAddress (sNetworkId @n) a
