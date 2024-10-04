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
import Cardano.Wallet.UI.Common.Html.Lib
    ( AlertH
    , truncatableText
    )
import Cardano.Wallet.UI.Common.Html.Pages.Lib
    ( Striped (..)
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
import Cardano.Wallet.UI.Deposit.Html.Lib
    ( selectCustomerH
    )
import Cardano.Wallet.UI.Deposit.Html.Pages.Addresses.Transactions
    ( transactionsElementH
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
    )

addressesH :: WHtml ()
addressesH = do
    sseH addressesLink "addresses" ["wallet"]

customerAddressH :: Monad m => Address -> HtmlT m ()
customerAddressH addr = truncatableText "address-text" $ toHtml encodedAddr
  where
    encodedAddr = encodeMainnetAddress addr

addressElementH :: UTCTime -> UTCTime -> AlertH -> WalletPresent -> Html ()
addressElementH now origin = onWalletPresentH $ \case
    WalletPublicIdentity _xpub customers ->
        div_ [id_ "view-control"] $ do
            div_ [class_ "row mt-2 g-0"] $ do
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
