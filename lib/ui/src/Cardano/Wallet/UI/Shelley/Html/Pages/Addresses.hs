{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Shelley.Html.Pages.Addresses
    ( addressesPageH
    , addressesH
    )
where

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
import Cardano.Wallet.UI.Common.Html.Copy
    ( copyButton
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( Striped (..)
    , Width (..)
    , fieldHtml
    , record
    , simpleField
    , sseH
    )
import Cardano.Wallet.UI.Shelley.API
    ( walletAddressesLink
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
    ( HtmlT
    , ToHtml (toHtml)
    , class_
    , div_
    , id_
    , li_
    , ul_
    )

addressesPageH :: Monad m => HtmlT m ()
addressesPageH =
    sseH walletAddressesLink "addresses" ["wallet"]

addressesH
    :: forall n m
     . (HasSNetworkId n, Monad m)
    => [ApiAddressWithPath n]
    -> HtmlT m ()
addressesH addresses = record Nothing Full Striped $ do
    forM_ (zip [0 :: Int ..] addresses) $ \(j, ApiAddressWithPath{..}) -> do
        fieldHtml [] "id" $ do
            let identifier = "address-" <> toText j
            div_ [class_ "row"] $ do
                div_ [class_ "text-break col-sm-10", id_ identifier]
                    $ toHtml
                    $ addressH (Proxy @n)
                    $ apiAddress id
                div_ [class_ "col-sm-2"] $ toHtml $ copyButton identifier
        simpleField "state" $ toHtml $ toText $ getApiT state
        fieldHtml [] "derivation path"
            $ ul_ [class_ "list-inline"]
            $ forM_ derivationPath
            $ li_ [class_ "list-inline-item"]
                . toHtml
                . toText
                . getApiT

addressH
    :: forall n m
     . (HasSNetworkId n, Monad m)
    => Proxy n
    -> Address
    -> HtmlT m ()
addressH _ a = toHtml $ encodeAddress (sNetworkId @n) a
