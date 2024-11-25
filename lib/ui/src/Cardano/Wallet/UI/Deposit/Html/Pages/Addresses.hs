{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.UI.Deposit.Html.Pages.Addresses
where

import Prelude

import Cardano.Wallet.Deposit.IO
    ( WalletPublicIdentity (..)
    )
import Cardano.Wallet.Deposit.Pure.API.Address
    ( encodeAddress
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Cardano.Wallet.UI.Common.Html.Htmx
    ( hxInclude_
    , hxPost_
    , hxTarget_
    , hxTrigger_
    )
import Cardano.Wallet.UI.Common.Html.Lib
    ( AlertH
    , WithCopy
    , linkText
    , truncatableText
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( AssocRow
    , Striped (..)
    , Width (..)
    , box
    , record
    , simpleField
    , sseH
    )
import Cardano.Wallet.UI.Deposit.API
    ( addressesLink
    , customerAddressLink
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Addresses.Transactions
    ( transactionsElementH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Wallet
    ( WalletPresent (..)
    , onWalletPresentH
    )
import Cardano.Wallet.UI.Lib.ListOf
    ( ListOf
    )
import Cardano.Wallet.UI.Type
    ( WHtml
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
import Servant
    ( Link
    )

addressesH :: WHtml ()
addressesH = do
    sseH addressesLink "addresses" ["wallet"]

customerAddressH :: Monad m => WithCopy -> Address -> HtmlT m ()
customerAddressH copy addr =
    truncatableText copy ("address-text-" <> encodedAddr)
        $ toHtml encodedAddr
  where
    encodedAddr = encodeAddress addr

addressElementH
    :: UTCTime -> UTCTime -> AlertH -> WalletPresent -> Html ()
addressElementH now origin = onWalletPresentH $ \case
    WalletPublicIdentity _xpub customers ->
        div_ [id_ "view-control"] $ do
            div_ [class_ "row mt-2 gx-0"] $ do
                box "Selection" mempty
                    $ div_ [class_ "col"]
                    $ record (Just 11) Full Striped
                    $ do
                        selectCustomerH
                            "#customer-address"
                            (Just "#view-control")
                            customerAddressLink
                            $ fromIntegral customers
                        simpleField "Address"
                            $ div_
                                [ id_ "customer-address"
                                ]
                                mempty
            transactionsElementH now origin

selectCustomerH
    :: Monad m
    => Text
    -- ^ CSS selector for the target element
    -> Maybe Text
    -- ^ HTMX include
    -> Link
    -- ^ post link
    -> Int
    -- ^ Number of tracked users
    -> ListOf (AssocRow m)
selectCustomerH identifier include link trackedUsers =
    simpleField "Customer Number"
        $ div_ [class_ "d-flex justify-content-end align-items-center"]
        $ input_
        $ [ id_ "select-customer"
          , type_ "number"
          , hxTarget_ identifier
          , class_ "form-control m-1 p-1"
          , hxTrigger_ "load, change"
          , hxPost_ $ linkText link
          , min_ "0"
          , max_ $ toText $ trackedUsers - 1
          , step_ "1"
          , name_ "customer"
          , value_ "0"
          , size_ "5"
          , style_ "width: 7em"
          ]
            <> maybe [] (\x -> [hxInclude_ x]) include
